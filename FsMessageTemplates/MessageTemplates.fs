module FsMessageTemplates.MessageTemplates

open System.Text
open System.IO

type DestrHint = Default = 0 | Stringify = 1 | Destructure = 2
type Direction = Left = 0 | Right = 1

[<Struct>]
type AlignInfo(d:Direction, w:int) =
    member __.Direction = d
    member __.Width = w
    static member Default = AlignInfo(Direction.Left, 0)
    override x.ToString() = (if x.Direction = Direction.Right then "-" else "") + string x.Width

let inline getDestrHintChar destr = match destr with DestrHint.Destructure -> "@" | DestrHint.Stringify -> "$" | _ -> ""
let inline getDestrFromChar c = match c with '@' -> DestrHint.Destructure | '$' -> DestrHint.Stringify | _ -> DestrHint.Default
let inline stringOrNone (v:'a option) = match v with Some x -> string x | None -> ""
type System.Text.StringBuilder with
    member this.AppendIf (cond:bool, s:string) = if cond then this.Append(s) else this

[<Struct>]
type PropertyToken(name:string, pos:int option, destr:DestrHint, align: AlignInfo option, format: string option) =
    static member Empty = PropertyToken("", None, DestrHint.Default, None, None)
    member __.Name = name
    member __.Pos = pos
    member __.Destr = destr
    member __.Align = align
    member __.Format = format
    member x.IsPositional with get() = x.Pos.IsSome
    member private x.ToPropertyString (includeDestr:bool, name:string) =
        let sb = StringBuilder()
                    .Append("{")
                    .AppendIf(includeDestr && x.Destr <> DestrHint.Default, getDestrHintChar x.Destr)
                    .Append(name)
                    .AppendIf(x.Align <> None, "," + (stringOrNone x.Align))
                    .AppendIf(x.Format <> None, ":" + (stringOrNone x.Format))
                    .Append("}")
        sb.ToString()

    member x.ToStringFormatTemplate(fmtPos: int option) = x.ToPropertyString(false, string (defaultArg fmtPos 0))
    override x.ToString() = x.ToPropertyString(true, x.Name)

type Token =
| Text of startIndex:int * text:string
| Prop of startIndex:int * PropertyToken
    static member EmptyText = Text(0, "")
    static member EmptyProp = Prop(0, PropertyToken.Empty)
    override x.ToString() = match x with | Text (_, s) -> s | Prop (_, pd) -> (string pd)

[<Struct; StructuralEquality; StructuralComparison>]
type Template =
    val Tokens : Token list
    val FormatString : string
    val Properties : PropertyToken list
    val internal Named : PropertyToken list
    val internal PositionalsByPos : PropertyToken list
    new (text:string, tokens: Token list) = 
        let properties = tokens |> List.choose (function Prop (_,pd) -> Some pd | _ -> None)
        let positionalProps, namedProps =
            properties |> List.partition (fun pd -> pd.IsPositional)
        let positionalsByPos = positionalProps |> List.sortBy (fun pd -> pd.Pos)
        { FormatString = text; Tokens = tokens
          Properties = properties; Named=namedProps; PositionalsByPos=positionalsByPos; }

type ScalarKeyValuePair = obj * TemplatePropertyValue
and PropertyNameAndValue = string * TemplatePropertyValue
and TemplatePropertyValue =
| ScalarValue of obj
| SequenceValue of TemplatePropertyValue seq
| StructureValue of typeTag:string option * values:PropertyNameAndValue list
| DictionaryValue of data: ScalarKeyValuePair seq

type Destructurer = DestructureRequest -> TemplatePropertyValue option
and
    [<Struct>]
    DestructureRequest(hint:DestrHint, value:obj, destr:Destructurer) =
        member x.Hint = hint
        member x.Value = value
        member x.Destr = destr

type PropertyAndValue = PropertyToken * TemplatePropertyValue

let inline tryParseInt32 s = System.Int32.TryParse(s, System.Globalization.NumberStyles.None, System.Globalization.CultureInfo.InvariantCulture)
let inline maybeInt32 s =
    if System.String.IsNullOrEmpty(s) then None
    else match tryParseInt32 s with true, n -> Some n | false, _ -> None

let parseTextToken (startAt:int) (template:string) : int*Token =
    let chars = template
    let tlen = chars.Length
    let sb = StringBuilder()
    let inline append (ch:char) = sb.Append(ch) |> ignore
    let rec go i =
        if i >= tlen then tlen, Text(startAt, sb.ToString())
        else
            let c = chars.[i]
            match c with
            | '{' -> if (i+1) < tlen && chars.[i+1] = '{' then append c; go (i+2)
                     elif i = startAt then startAt, Token.EmptyText
                     else i, Text(startAt, sb.ToString())
            | '}' when (i+1) < tlen && chars.[i+1] = '}' -> append c; go (i+2)
            | _ -> append c; go (i+1)
    go startAt

let inline isLetterOrDigit c = System.Char.IsLetterOrDigit c
let inline isValidInPropName c = c = '_' || System.Char.IsLetterOrDigit c
let inline isValidInDestrHint c = c = '@' || c = '$'
let inline isValidInAlignment c = c = '-' || System.Char.IsDigit c
let inline isValidInFormat c = c <> '}' && (c = ' ' || isLetterOrDigit c || System.Char.IsPunctuation c)
let inline isValidCharInPropTag c = c = ':' || isValidInPropName c || isValidInFormat c || isValidInDestrHint c
let inline tryGetFirstChar predicate (s:string) first =
    let len = s.Length
    let rec go i =
        if i >= len then None
        else if not (predicate s.[i]) then go (i+1) else Some i
    go first

[<Struct>]
type Range(startIndex:int, endIndex:int) =
    member this.Start = startIndex
    member this.End = endIndex
    member this.Length = (endIndex - startIndex) + 1
    member this.GetSubString (s:string) = s.Substring(startIndex, this.Length)
    member this.IncreaseBy startNum endNum = Range(startIndex+startNum, endIndex+endNum)
    member this.Right (startFromIndex:int) =
        if startFromIndex < startIndex then invalidArg "startFromIndex" "startFromIndex must be >= Start"
        Range(startIndex, this.End)
    override __.ToString() = (string startIndex) + ", " + (string endIndex)

let inline tryGetFirstCharRng predicate (s:string) (rng:Range) =
    let rec go i =
        if i > rng.End then None
        else if not (predicate s.[i]) then go (i+1) else Some i
    go rng.Start

let inline hasAnyInvalidRng isValid (s:string) (rng:Range) =
    match tryGetFirstChar (not<<isValid) s rng.Start with
    | Some i -> i <= rng.End
    | None -> false

let inline hasAnyInvalid isValid (s:string) =
    hasAnyInvalidRng isValid s (Range(0, s.Length - 1))

/// just like System.String.IndexOf(char) but within a string range
let inline rngIndexOf (s:string) (rng:Range) (c:char) : int =
    match tryGetFirstCharRng ((=) c) s rng with None -> -1 | Some i -> i

let tryParseAlignInfoRng (s:string) (rng:Range option) : bool * AlignInfo option =
    match s, rng with
    | _, None -> true, None
    | s, Some rng when (rng.Start >= rng.End) || (hasAnyInvalidRng isValidInAlignment s rng) -> false, None
    | s, Some rng ->
        let alignSubString = rng.GetSubString s
        let lastDashIdx = alignSubString.LastIndexOf('-')
        let width = match lastDashIdx with
                    | 0 -> int (alignSubString.Substring(1)) // skip dash for the numeric alignment
                    | -1 -> int alignSubString // no dash, must be all numeric
                    | _ -> 0 // dash is not allowed to be anywhere else
        if width = 0 then false, None
        else
            let direction = match lastDashIdx with -1 -> Direction.Left | _ -> Direction.Right
            true, Some (AlignInfo(direction, width))

let inline tryGetPropInSubString (t:string) (within : Range) : Token option =
    /// Given a template such has "Hello, {@name,-10:abc}!" and a *within* range
    /// of Start=8, End=19 (i.e. the full 'insides' of the property tag between { and },
    /// this returns the Start/End pairs of the Name, Align, and Format components. If
    /// anything 'invalid' is found then the first Range is a value of None.
    let nameRange, alignRange, formatRange =
        match (rngIndexOf t within ','), (rngIndexOf t within ':') with
        | -1, -1 -> Some within, None, None // neither align nor format
        | -1, fmtIdx -> Some (Range(within.Start, fmtIdx-1)), None, Some (Range(fmtIdx+1, within.End)) // has format part, but does not have align part
        | alIdx, -1 -> Some (Range(within.Start, alIdx-1)), Some (Range(alIdx+1, within.End)), None // has align part, but does not have format part
        | alIdx, fmtIdx when alIdx < fmtIdx && alIdx <> (fmtIdx - 1) -> // has both parts in correct order
            let align = Some (Range(alIdx+1, fmtIdx-1))
            let fmt = Some (Range(fmtIdx+1, within.End))
            Some (Range(within.Start, alIdx-1)), align, fmt
        | alIdx, fmtIdx when alIdx > fmtIdx ->
            Some (Range(within.Start, fmtIdx-1)), None, Some (Range(fmtIdx+1, within.End)) // has format part, no align (but one or more commas *inside* the format string)
        | _, _ -> None, None, None // hammer time; you can't split this

    match nameRange, alignRange, formatRange with
    | None, _, _ -> None
    | Some nameAndDestr, _, _ ->
        let destr = getDestrFromChar (t.[nameAndDestr.Start])
        let propertyName = match destr with
                           | DestrHint.Default -> nameAndDestr.GetSubString t
                           | _ -> Range(nameAndDestr.Start+1, nameAndDestr.End).GetSubString t

        if propertyName = "" || (hasAnyInvalid isValidInPropName propertyName) then None
        elif formatRange.IsSome && (hasAnyInvalidRng isValidInFormat t formatRange.Value) then None
        else match (tryParseAlignInfoRng t alignRange) with
             | false, _ -> None
             | true, alignInfo ->
                let format = formatRange |> Option.map (fun rng -> rng.GetSubString t)
                Some (Prop(within.Start - 1,
                           PropertyToken(propertyName, maybeInt32 propertyName, destr, alignInfo, format)))

let parsePropertyToken (startAt:int) (mt:string) : int*Token =
    let tlen = mt.Length
    let inline getc idx = mt.[idx]
    let first = startAt

    // skip over characters after the open-brace, until we reach a character that
    // is *NOT* a valid part of the property tag. this will be the close brace character
    // if the template is actually a well-formed property tag.
    let nextInvalidCharIndex =
        match tryGetFirstChar (not << isValidCharInPropTag) mt (first+1) with
        | Some idx -> idx
        | None -> tlen

    // if we stopped at the end of the string or the last char wasn't a close brace
    // then we treat all the characters we found as a text token, and finish.
    if nextInvalidCharIndex = tlen || getc nextInvalidCharIndex <> '}' then
        nextInvalidCharIndex, Token.Text(first, mt.Substring(first, nextInvalidCharIndex - first))
    else
        // skip over the trailing "}" close prop tag
        let nextIndex = nextInvalidCharIndex + 1
        let propInsidesRng = Range(first + 1, nextIndex - 2)
        match tryGetPropInSubString mt propInsidesRng with
        | Some p -> nextIndex, p
        | None _ -> nextIndex, Token.Text(first, (mt.Substring(first, nextIndex - first)))

let emptyTextTokenList = [ Token.EmptyText ]

let parseTokens (template:string) : Token list =
    let tlen = template.Length
    if tlen = 0 then emptyTextTokenList
    else
        let rec go start acc =
            if start >= tlen then List.rev acc
            else match parseTextToken start template with
                 | next, tok when next <> start -> go next (tok::acc)
                 | next, _ -> // no token parsed
                    match parsePropertyToken start template with
                    | nextFromProp, tok when nextFromProp <> start -> go nextFromProp (tok::acc)
                    | nextFromProp, _ -> List.rev acc
        go 0 []

let parse (s:string) = Template(s, s |> parseTokens)

module Destructure =
    open System
    type DtOffset = System.DateTimeOffset

    let inline tryCastAs<'T> (o:obj) =  match o with | :? 'T as res -> Some res | _ -> None
    
    let scalarTypes = 
        [ typeof<bool>;      typeof<char>;       typeof<byte>;      typeof<int16>
          typeof<uint16>;    typeof<int32>;      typeof<uint32>;    typeof<int64>
          typeof<uint64>;    typeof<single>;     typeof<double>;    typeof<decimal>
          typeof<string>;    typeof<DateTime>;   typeof<DtOffset>;  typeof<TimeSpan>
          typeof<Guid>;      typeof<Uri>; ]

    let scalarTypeHash = System.Collections.Generic.HashSet(scalarTypes)

    let inline tryBuiltInTypes (r:DestructureRequest) =
        if scalarTypeHash.Contains(r.Value.GetType()) then Some (ScalarValue r.Value)
        else None

    let inline tryNullable (r:DestructureRequest) =
        let t = r.Value.GetType()
        let isNullable = t.IsConstructedGenericType && t.GetGenericTypeDefinition() = typeof<Nullable<_>>
        if isNullable then
            match tryCastAs<System.Nullable<_>>() with
            | Some n when n.HasValue -> r.Destr (DestructureRequest(r.Hint, box (n.GetValueOrDefault()), r.Destr)) // re-destructure with the value inside
            | Some n when (not n.HasValue) -> Some (ScalarValue null)
            | _ -> None
        else None

    let inline tryEnum (r:DestructureRequest) =
        match tryCastAs<System.Enum>(r.Value) with
        | Some e -> Some (ScalarValue (e))
        | None -> None

    let inline tryByteArrayMaxBytes (maxBytes:int) (r:DestructureRequest) =
        match tryCastAs<System.Byte[]>(r.Value) with
        | Some bytes when bytes.Length <= maxBytes -> Some (ScalarValue bytes)
        | Some bytes when bytes.Length > maxBytes ->
            let inline toHexString (b:byte) = b.ToString("X2")
            let start = bytes |> Seq.take maxBytes |> Seq.map toHexString |> String.Concat
            let description = start + "... (" + string bytes.Length + " bytes)"
            Some (ScalarValue (description))
        | _ -> None

    let inline tryByteArray r = tryByteArrayMaxBytes 1024 r

    let inline tryReflectionTypes (r:DestructureRequest) =
        match r.Value with
        | :? Type as t -> Some (ScalarValue t)
        | :? System.Reflection.MemberInfo as m -> Some (ScalarValue m)
        | _ -> None

    let inline tryScalarDestructure (r:DestructureRequest) =
        [ tryBuiltInTypes; tryNullable; tryEnum; tryByteArray; tryReflectionTypes ]
        |> List.tryPick (fun tryDestr -> tryDestr r)
    
    let inline tryNull (r:DestructureRequest) =
        match r.Value with | null -> Some (ScalarValue null) | _ -> None
    let inline tryStringifyDestructurer (r:DestructureRequest) =
        match r.Hint with | DestrHint.Stringify -> Some (ScalarValue (r.Value.ToString())) | _ -> None
    let inline tryDestructuringDestr (r:DestructureRequest)  = None // TODO:
    let inline tryEnumerableDestr (r:DestructureRequest) = None // TODO:
    let inline scalarStringCatchAllDestr (r:DestructureRequest) = Some (ScalarValue (r.Value.ToString()))

    // Doesn't it look pretty?
    let inline tryAll r =
        match tryNull r with
        |Some _ as tpv -> tpv
        |_->match tryStringifyDestructurer r with
            |Some _ as tpv -> tpv
            |_->match tryScalarDestructure r with
                |Some _ as tpv -> tpv
                |_->match tryDestructuringDestr r with
                    |Some _ as tpv -> tpv
                    |_->match tryEnumerableDestr r with
                        |Some _ as tpv -> tpv
                        |_->match scalarStringCatchAllDestr r with
                            |Some _ as tpv -> tpv
                            | _->None

type SelfLogger = (string * obj[]) -> unit
let inline nullLogger (format: string, values: obj[]) = ()

/// 'Zips' the propertes and values the destructure each value, only returning
/// those that were destructured.
let inline zipDestr (destr:Destructurer) (props:PropertyToken list) (values:obj[]) =
    props
    |> Seq.zip values // because it ignores extra elements from the larger sequence
    |> Seq.choose (fun (v, pt) ->
        let req = DestructureRequest(pt.Destr, v, destr)
        destr req |> Option.map (fun tpv -> pt, tpv))
    |> Seq.toList

let inline capturePropertiesWith (log:SelfLogger) (d:Destructurer) (t:Template) (args: obj[]) =
    let anyProps = (not t.Properties.IsEmpty)
    if (args = null || args.Length = 0) && anyProps then List.empty
    elif not anyProps then List.empty
    else
        if not (t.PositionalsByPos.IsEmpty) && t.Named.IsEmpty then
            zipDestr d t.PositionalsByPos args
        else
            zipDestr d t.Properties args

let captureProperties (t:Template) (args:obj[]) =
    capturePropertiesWith nullLogger Destructure.tryAll t args
    |> List.map (fun (pt, tpv) -> pt.Name, tpv)

let captureMessageProperties (s:string) (args:obj[]) = captureProperties (s |> parse) args

let rec writePropToken  (w: TextWriter) (pt: PropertyToken) (pv: TemplatePropertyValue) =
    match pv with
    | ScalarValue null -> w.Write "null"
    | ScalarValue sv ->
        let ptFormatString = pt.ToStringFormatTemplate(None)
        match sv with
        | :? string as s -> w.Write("\""+ptFormatString+"\"", s.Replace("\"", "\\\""))
        | _ -> w.Write(ptFormatString, [| sv |])
    | SequenceValue svs -> for sv in svs do writePropToken w pt sv
    | StructureValue(typeTag, values) ->
        typeTag |> Option.iter (fun s -> w.Write s)
        w.Write " { "
        for (n, v) in values do w.Write(" {0} = ", n); writePropToken w pt v; w.Write " "
        w.Write " } "
    | DictionaryValue(data) -> failwith "Not implemented yet"

/// Converts template token and value into a rendered string.
let inline writeToken (w: TextWriter) (writePropToken) (token:Token) (value:TemplatePropertyValue option) =
    match token, value with
    | Token.Text (_, raw), None -> w.Write raw
    | Token.Prop (_, pt), Some pv -> writePropToken w pt pv
    | Token.Prop (_, pt), None -> w.Write (pt.ToStringFormatTemplate(pt.Pos))
    | Token.Text (_, raw), Some pv -> failwithf "unexpected text token %s with property value %A" raw pv

let doFormat (w: TextWriter) (writePropToken) (template:Template) (values:obj[]) =
    let valuesByPropName = captureProperties template values
    for t in template.Tokens do
        match t with
        | Token.Text _ as tt -> writeToken w writePropToken tt None
        | Token.Prop (_, pd) as tp ->
            let value = valuesByPropName
                        |> List.tryPick (function nme, tpv when nme = pd.Name -> Some tpv | _ -> None)
            writeToken w writePropToken tp value

let format provider template values =
    use tw = new System.IO.StringWriter(formatProvider=provider)
    doFormat tw writePropToken template values
    tw.ToString()

let bprintn (sb:System.Text.StringBuilder) (template:string) (args:obj[]) = () // TODO:
let sfprintn (p:System.IFormatProvider) (template:string) (args:obj[]) = "" // TODO:
let fprintn (tw:System.IO.TextWriter) (template:string) (args:obj[]) = () // TODO:
