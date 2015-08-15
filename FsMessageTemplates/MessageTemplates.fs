module FsMessageTemplates.MessageTemplates

open System.Text
open System.IO

type DestrHint = Default = 0 | Stringify = 1 | Destructure = 2
type Direction = Left = 0 | Right = 1

[<Struct>]
type AlignInfo =
    new (direction:Direction, width:int) = { _direction=direction; _width=width; }
    new (isValid:bool) = { _direction=Direction.Left; _width=(if isValid then -1 else -2) }
    val private _direction:Direction
    val private _width:int
    member this.Direction with get() = this._direction
    member this.Width with get() = this._width
    member this.IsEmpty = this.Width = -1
    member internal this.IsValid = this.Width <> -2
    static member Empty = AlignInfo(isValid=true)
    static member Invalid = AlignInfo(isValid=false)
    override x.ToString() = if x.IsEmpty then "" elif x.IsValid = false then ""
                            else (if x._direction = Direction.Right then "-" else "") + string x._width

let inline getDestrHintChar destr = if destr = DestrHint.Default then "" elif destr = DestrHint.Destructure then "@" else "$"
let inline getDestrFromChar c = if c = '@' then DestrHint.Destructure elif c = '$' then DestrHint.Stringify else DestrHint.Default
let inline stringOrNone (v:'a option) = match v with Some x -> string x | None -> ""
type System.Text.StringBuilder with
    member inline this.AppendIf (cond:bool, s:string) = if cond then this.Append(s) else this
    member inline this.ToStringAndClear () = let s = this.ToString() in this.Clear()|>ignore; s

[<Struct>]
type PropertyToken(name:string, pos:int, destr:DestrHint, align: AlignInfo, format: string option) =
    static member Empty = PropertyToken("", -1, DestrHint.Default, AlignInfo.Empty, None)
    member __.Name = name
    member __.Pos = pos
    member __.Destr = destr
    member __.Align = align
    member __.Format = format
    member x.IsPositional with get() = x.Pos >= 0
    member private x.ToPropertyString (sb:StringBuilder, includeDestr:bool, name:string) =
        sb  .Append("{")
            .AppendIf(includeDestr && x.Destr <> DestrHint.Default, getDestrHintChar x.Destr)
            .Append(name)
            .AppendIf(not x.Align.IsEmpty, "," + (string x.Align))
            .AppendIf(x.Format <> None, ":" + (stringOrNone x.Format))
            .Append("}")
            .ToStringAndClear()

    member x.ToStringFormatTemplate(sb:StringBuilder, fmtPos: int) = x.ToPropertyString(sb, false, if fmtPos = 0 then "0" else string fmtPos)
    override x.ToString() = x.ToPropertyString(StringBuilder(), true, x.Name)

type Token =
| Text of startIndex:int * text:string
| Prop of startIndex:int * PropertyToken
    override x.ToString() = match x with | Text (_, s) -> s | Prop (_, pd) -> (string pd)

let emptyTextToken = Text(0, "")
let emptyPropToken = Prop(0, PropertyToken.Empty)

type Template(formatString:string, tokens: Token[], isNamed:bool, properties:PropertyToken[]) =
    member this.Tokens = tokens :> Token seq
    member this.FormatString = formatString
    member this.Properties = properties :> PropertyToken seq
    member internal this.Named = if isNamed then properties else Unchecked.defaultof<PropertyToken[]>
    member internal this.PositionalsByPos = if isNamed then Unchecked.defaultof<PropertyToken[]> else properties
    member internal this.HasAnyProperties = properties.Length > 0

type ScalarKeyValuePair = TemplatePropertyValue * TemplatePropertyValue
and PropertyNameAndValue = string * TemplatePropertyValue
and TemplatePropertyValue =
| ScalarValue of obj
| SequenceValue of TemplatePropertyValue list
| StructureValue of typeTag:string * values:PropertyNameAndValue []
| DictionaryValue of data: ScalarKeyValuePair list

type Destructurer = DestructureRequest -> TemplatePropertyValue
and
    [<Struct>]
    DestructureRequest(hint:DestrHint, value:obj, destr:Destructurer) =
        member x.Hint = hint
        member x.Value = value
        member x.Destr = destr
        member internal x.WithValue (newValue:obj) = DestructureRequest(hint, newValue, destr)
        member internal x.TryAgainWithValue (newValue:obj) = destr (x.WithValue newValue)

type PropertyAndValue = PropertyToken * TemplatePropertyValue

let inline tryParseInt32 s = System.Int32.TryParse(s, System.Globalization.NumberStyles.None, System.Globalization.CultureInfo.InvariantCulture)
let inline parseIntOrNegative1 s =
    if System.String.IsNullOrEmpty(s) then -1
    else match tryParseInt32 s with true, n -> n | _ -> -1

let inline parseTextToken (startAt:int) (template:string) : int*Token =
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
                     elif i = startAt then startAt, emptyTextToken
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
        if i >= len then -1
        else if not (predicate s.[i]) then go (i+1) else i
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
        if i > rng.End then -1
        else if not (predicate s.[i]) then go (i+1) else i
    go rng.Start

let inline hasAnyInvalidRng isValid (s:string) (rng:Range) =
    match tryGetFirstChar (not<<isValid) s rng.Start with
    | -1 -> false | i -> i <= rng.End

let inline hasAnyInvalid isValid (s:string) =
    hasAnyInvalidRng isValid s (Range(0, s.Length - 1))

/// just like System.String.IndexOf(char) but within a string range
let inline rngIndexOf (s:string) (rng:Range) (c:char) = tryGetFirstCharRng ((=) c) s rng 

let inline tryParseAlignInfoRng (s:string) (rng:Range option) : AlignInfo =
    match s, rng with
    | _, None -> AlignInfo(isValid=true)
    | s, Some rng when (rng.Start >= rng.End) || (hasAnyInvalidRng isValidInAlignment s rng) -> AlignInfo(isValid=false)
    | s, Some rng ->
        let alignSubString = rng.GetSubString s
        let lastDashIdx = alignSubString.LastIndexOf('-')
        let width = match lastDashIdx with
                    | 0 -> int (alignSubString.Substring(1)) // skip dash for the numeric alignment
                    | -1 -> int alignSubString // no dash, must be all numeric
                    | _ -> 0 // dash is not allowed to be anywhere else
        if width = 0 then AlignInfo(isValid=false)
        else
            let direction = match lastDashIdx with -1 -> Direction.Left | _ -> Direction.Right
            AlignInfo(direction, width)

let inline tryGetPropInSubString (t:string) (within : Range) : Token =
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
    | None, _, _ -> emptyPropToken
    | Some nameAndDestr, _, _ ->
        let destr = getDestrFromChar (t.[nameAndDestr.Start])
        let propertyName = match destr with
                           | DestrHint.Default -> nameAndDestr.GetSubString t
                           | _ -> Range(nameAndDestr.Start+1, nameAndDestr.End).GetSubString t

        if propertyName = "" || (hasAnyInvalid isValidInPropName propertyName) then emptyPropToken
        elif formatRange.IsSome && (hasAnyInvalidRng isValidInFormat t formatRange.Value) then emptyPropToken
        else match (tryParseAlignInfoRng t alignRange) with
             | ai when ai.IsValid = false -> emptyPropToken
             | alignInfo ->
                let format = formatRange |> Option.map (fun rng -> rng.GetSubString t)
                Prop(within.Start - 1,
                     PropertyToken(propertyName, parseIntOrNegative1 propertyName, destr, alignInfo, format))

let emptyTextTokenArray = [| emptyTextToken |]

let parseTokens (mt:string) =
    let tlen = mt.Length
    if tlen = 0 then emptyTextTokenArray
    else
        let rz = ResizeArray<Token>()
        let rec go start =
            if start >= tlen then rz.ToArray()
            else match parseTextToken start mt with
                 | next, tok when next <> start ->
                    rz.Add tok; go next
                 | _, _ -> // no text token parsed, try a property
                    let inline getc idx = mt.[idx]
                    let first = start

                    // skip over characters after the open-brace, until we reach a character that
                    // is *NOT* a valid part of the property tag. this will be the close brace character
                    // if the template is actually a well-formed property tag.
                    let nextInvalidCharIndex =
                        match tryGetFirstChar (not << isValidCharInPropTag) mt (first+1) with
                        | -1 -> tlen | idx -> idx

                    // if we stopped at the end of the string or the last char wasn't a close brace
                    // then we treat all the characters we found as a text token, and finish.
                    if nextInvalidCharIndex = tlen || getc nextInvalidCharIndex <> '}' then
                        rz.Add (Token.Text(first, mt.Substring(first, nextInvalidCharIndex - first)))
                        go nextInvalidCharIndex
                    else
                        // skip over the trailing "}" close prop tag
                        let nextIndex = nextInvalidCharIndex + 1
                        let propInsidesRng = Range(first + 1, nextIndex - 2)
                        match tryGetPropInSubString mt propInsidesRng with
                        | p when obj.ReferenceEquals(p, emptyPropToken) ->
                            rz.Add (Token.Text(first, (mt.Substring(first, nextIndex - first))))
                            go nextIndex
                        | p -> rz.Add p; go nextIndex
        go 0

let parse (s:string) =
    let tokens = s |> parseTokens
    let properties = tokens |> Array.choose (function Prop (_, pt) -> Some pt | _ -> None)
    let mutable allPos, anyPos = true, false
    for i = 0 to (properties.Length-1) do
        if properties.[i].IsPositional then anyPos <- true else allPos <- false
    if allPos then properties |> Array.sortInPlaceBy (fun p -> p.Pos)
    Template(s, tokens, not allPos, properties)

module Destructure =
    open System
    type DtOffset = System.DateTimeOffset

    // perf
    let emptyKeepTrying = Unchecked.defaultof<TemplatePropertyValue>
    let inline isEmptyKeepTrying (tpv:TemplatePropertyValue) = Object.ReferenceEquals(tpv, null)
    let inline tryCastAs<'T> (o:obj) =  match o with | :? 'T as res -> res | _ -> Unchecked.defaultof<'T>
    
    let scalarTypes = 
        [ typeof<bool>;      typeof<char>;       typeof<byte>;      typeof<int16>
          typeof<uint16>;    typeof<int32>;      typeof<uint32>;    typeof<int64>
          typeof<uint64>;    typeof<single>;     typeof<double>;    typeof<decimal>
          typeof<string>;    typeof<DateTime>;   typeof<DtOffset>;  typeof<TimeSpan>
          typeof<Guid>;      typeof<Uri>; ]

    let scalarTypeHash = System.Collections.Generic.HashSet(scalarTypes)

    let inline tryBuiltInTypesOrNull (r:DestructureRequest) =
        if r.Value = null then ScalarValue null 
        elif scalarTypeHash.Contains(r.Value.GetType()) then (ScalarValue r.Value)
        else emptyKeepTrying

    let inline tryNullable (r:DestructureRequest) =
        let t = r.Value.GetType()
        let isNullable = t.IsConstructedGenericType && t.GetGenericTypeDefinition() = typedefof<Nullable<_>>
        if isNullable then
            match tryCastAs<System.Nullable<_>>(r.Value) with
            | n when (Object.ReferenceEquals(null, n)) -> (ScalarValue null)
            | n when (not n.HasValue) -> (ScalarValue null)
            | n when n.HasValue -> r.TryAgainWithValue(box (n.GetValueOrDefault()))
            | _ -> emptyKeepTrying
        else emptyKeepTrying

    let inline tryEnum (r:DestructureRequest) =
        match tryCastAs<System.Enum>(r.Value) with
        | e when (Object.ReferenceEquals(null, e)) -> emptyKeepTrying
        | e -> (ScalarValue (e))

    let inline tryByteArrayMaxBytes (maxBytes:int) (r:DestructureRequest) =
        match tryCastAs<System.Byte[]>(r.Value) with
        | bytes when (Object.ReferenceEquals(null, bytes)) -> emptyKeepTrying
        | bytes when bytes.Length <= maxBytes -> ScalarValue bytes
        | bytes ->
            let inline toHexString (b:byte) = b.ToString("X2")
            let start = bytes |> Seq.take maxBytes |> Seq.map toHexString |> String.Concat
            let description = start + "... (" + string bytes.Length + " bytes)"
            ScalarValue (description)

    let inline tryByteArray r = tryByteArrayMaxBytes 1024 r

    let inline tryReflectionTypes (r:DestructureRequest) =
        match r.Value with
        | :? Type as t -> ScalarValue t
        | :? System.Reflection.MemberInfo as m -> ScalarValue m
        | _ -> emptyKeepTrying

    let private scalarDestructurers = [| tryBuiltInTypesOrNull; tryNullable; tryEnum;
                                         tryByteArray; tryReflectionTypes |]
    let private scalarDestructurersLength = scalarDestructurers.Length
    
    let rec loopTryAll (r:DestructureRequest) (ds:Destructurer[]) i stop =
        if i >= stop then emptyKeepTrying
        else
            match ds.[i] r with
            | tpv when isEmptyKeepTrying tpv -> loopTryAll r ds (i+1) stop
            | tpv -> tpv
    
    let inline tryScalarDestructure (r:DestructureRequest) =
        loopTryAll r scalarDestructurers 0 scalarDestructurersLength

    let inline tryNull (r:DestructureRequest) =
        match r.Value with | null -> ScalarValue null | _ -> emptyKeepTrying
    let inline tryStringifyDestructurer (r:DestructureRequest) =
        match r.Hint with | DestrHint.Stringify -> ScalarValue (r.Value.ToString()) | _ -> emptyKeepTrying

    let inline tryDelegateString (r:DestructureRequest) =
        if r.Hint <> DestrHint.Destructure then emptyKeepTrying
        else
            match tryCastAs<System.Delegate>(r.Value) with
            | e when (Object.ReferenceEquals(null, e)) -> emptyKeepTrying
            | e -> (ScalarValue (string e))
    
    open System.Reflection
    let inline getTypeInfo (t:Type) =
        System.Reflection.IntrospectionExtensions.GetTypeInfo t
    let inline isValidScalarDictionaryKeyType (t:Type) =
        scalarTypeHash.Contains(t) || (getTypeInfo t).IsEnum
    let inline isScalarDict (t: Type) =
        t.IsConstructedGenericType
        && t.GetGenericTypeDefinition() = typedefof<System.Collections.Generic.Dictionary<_,_>>
        && isValidScalarDictionaryKeyType(t.GenericTypeArguments.[0])

    let inline tryEnumerableDestr (r:DestructureRequest) =
        let valueType = r.Value.GetType()
        match tryCastAs<System.Collections.IEnumerable>(r.Value) with
        | e when Object.ReferenceEquals(null, e) -> emptyKeepTrying
        | e when isScalarDict valueType ->
            let mutable keyProp, valueProp = Unchecked.defaultof<PropertyInfo>, Unchecked.defaultof<PropertyInfo>
            let getKey o = if keyProp = null then keyProp <- o.GetType().GetRuntimeProperty("Key")
                           keyProp.GetValue(o)
            let getValue o = if valueProp = null then valueProp <- o.GetType().GetRuntimeProperty("Value")
                             valueProp.GetValue(o)
            let skvps = e |> Seq.cast<obj>
                          |> Seq.map (fun o -> getKey o, getValue o)
                          |> Seq.map (fun (key, value) -> tryScalarDestructure (r.WithValue key), r.TryAgainWithValue(value))
                          |> Seq.toList
            DictionaryValue skvps
        | e -> SequenceValue(e |> Seq.cast<obj> |> Seq.map r.TryAgainWithValue |> Seq.toList)

    let inline tryCustomDestructuring (r:DestructureRequest) = emptyKeepTrying // TODO:
    let inline scalarStringCatchAllDestr (r:DestructureRequest) = ScalarValue (r.Value.ToString())

    let inline isPublicInstanceReadProp (p:PropertyInfo) =
        p.CanRead && p.GetMethod.IsPublic && not (p.GetMethod.IsStatic) &&
            (p.Name <> "Item" || p.GetIndexParameters().Length = 0)

    let inline tryObjectStructureDestructuring (r:DestructureRequest) =
        if r.Hint <> DestrHint.Destructure then emptyKeepTrying
        else
            let ty = r.Value.GetType()
            let typeTag = match ty.Name with
                          | s when s.Length = 0 || not (Char.IsLetter s.[0]) -> null
                          | s -> s
            
            let pubprops = ty.GetRuntimeProperties() |> Seq.where isPublicInstanceReadProp |> Seq.toArray

            let values =
                pubprops |> Array.map (fun pi ->
                    // TODO: try/catch(TargetParameterCountException+log)
                    // TODO: try/catch(TargetInvocationException+log)
                    let propValue = pi.GetValue(r.Value)
                    // TODO: depth limiting
                    pi.Name, r.TryAgainWithValue(propValue)) // recursive

            StructureValue(typeTag, values)

    let private allDestrs = [|  tryNull; tryStringifyDestructurer;
                                tryScalarDestructure; tryCustomDestructuring; tryDelegateString
                                tryEnumerableDestr; tryObjectStructureDestructuring;
                                scalarStringCatchAllDestr; // if all else fails, convert to a string
                            |]
    let private allDestrsLength = allDestrs.Length

    let inline tryAll r = loopTryAll r allDestrs 0 allDestrsLength

type SelfLogger = (string * obj[]) -> unit
let inline nullLogger (format: string, values: obj[]) = ()

let capturePropertiesWith (log:SelfLogger) (destr:Destructurer) (t:Template) (args: obj[]) =
    if (args = null || args.Length = 0) && t.HasAnyProperties then Array.empty
    elif not t.HasAnyProperties then Array.empty
    else
        let props = if t.PositionalsByPos <> null then t.PositionalsByPos else t.Named
        let mutable matchedRun = props.Length
        if props.Length <> args.Length then
            matchedRun <- min props.Length args.Length
            log("property count does not match parameter count: {0}", [|t.FormatString|])
        let result = Array.zeroCreate<PropertyNameAndValue>(matchedRun)
        for i = 0 to matchedRun-1 do
            let p = props.[i]
            Array.set result i (p.Name, (destr (DestructureRequest(p.Destr, args.[i], destr))))
        result

let captureProperties (t:Template) (args:obj[]) =
    capturePropertiesWith nullLogger Destructure.tryAll t args
    :> PropertyNameAndValue seq

let captureMessageProperties (s:string) (args:obj[]) = captureProperties (s |> parse) args

let rec writePropValue (sb: StringBuilder) (w: TextWriter) (pt: PropertyToken) (pv: TemplatePropertyValue) =
    match pv with
    | ScalarValue null -> w.Write "null"
    | ScalarValue sv ->
        let ptFormatString = pt.ToStringFormatTemplate(sb, 0)
        match sv with
        | :? string as s -> w.Write "\""; w.Write (ptFormatString, s.Replace("\"", "\\\"")); w.Write "\"";
        | _ -> w.Write(ptFormatString, [| sv |])
    | SequenceValue svs ->
        w.Write '['
        let lastIndex = svs.Length - 1
        svs |> List.iteri (fun i sv ->
            writePropValue sb w pt sv
            if i <> lastIndex then w.Write ", "
        )
        w.Write ']'
    | StructureValue(typeTag, values) ->
        if typeTag <> null && typeTag.Length > 0 then w.Write typeTag; w.Write ' '
        w.Write "{ "
        let lastIndex = values.Length - 1
        for i = 0 to lastIndex do
            let n, v = values.[i]
            w.Write("{0}: ", n)
            writePropValue sb w pt v
            w.Write (if i = lastIndex then " " else ", ")
        w.Write "}"
    | DictionaryValue(data) ->
        w.Write '['
        data |> List.iter (fun (entryKey, entryValue) ->
            w.Write '('
            writePropValue sb w pt entryKey
            w.Write ": "
            writePropValue sb w pt entryValue
            w.Write ")"
        )
        w.Write ']'

/// Converts template token and value into a rendered string.
let inline writeToken (sb: StringBuilder) (w: TextWriter) (writePropValue) (token:Token) (value:TemplatePropertyValue option) =
    match token, value with
    | Token.Text (_, raw), None -> w.Write raw
    | Token.Prop (_, pt), Some pv -> writePropValue sb w pt pv
    | Token.Prop (_, pt), None -> w.Write (pt.ToStringFormatTemplate(sb, (max pt.Pos 0)))
    | Token.Text (_, raw), Some pv -> failwithf "unexpected text token %s with property value %A" raw pv

let doFormat (w: TextWriter) (writePropValue) (template:Template) (values:obj[]) =
    let valuesByPropName = captureProperties template values |> dict
    let sb = StringBuilder()
    for t in template.Tokens do
        match t with
        | Token.Text _ as tt -> writeToken sb w writePropValue tt None
        | Token.Prop (_, pd) as tp ->
            let exists, value = valuesByPropName.TryGetValue(pd.Name)
            writeToken sb w writePropValue tp (if exists then Some value else None)

let format template values =
    use tw = new StringWriter()
    doFormat tw writePropValue template values
    tw.ToString()

let bprintsm (sb:StringBuilder) (template:string) (args:obj[]) =
    use tw = new StringWriter(sb)
    doFormat tw writePropValue (parse template) args

let sprintsm (p:System.IFormatProvider) (template:string) (args:obj[]) =
    use tw = new StringWriter(p)
    doFormat tw writePropValue (parse template) args
    tw.ToString()

let fprintsm (tw:TextWriter) (template:string) (args:obj[]) =
    let template = parse template
    doFormat tw writePropValue template args

let bprintm (template:Template) (sb:StringBuilder) (args:obj[]) =
    use tw = new StringWriter(sb)
    doFormat tw writePropValue template args

let sprintm (template:Template) (provider:System.IFormatProvider) (args:obj[]) =
    use tw = new StringWriter(provider)
    doFormat tw writePropValue template args
    tw.ToString()

let fprintm (template:Template) (tw:TextWriter) (args:obj[]) =
    doFormat tw writePropValue template args
