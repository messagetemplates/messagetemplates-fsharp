module FsMessageTemplates.MessageTemplates

open System

type DestructureKind = Default = 0 | Stringify = 1 | Destructure = 2

let emptyTokenData = (0, "")

type Direction = Left = 0 | Right = 1

[<Struct>]
type AlignInfo(d:Direction, w:int) =
    member __.Direction = d
    member __.Width = w
    with static member Default = AlignInfo(Direction.Left, 0)
            override x.ToString() =
            let dir = if x.Direction = Direction.Right then "-" else ""
            dir + string x.Width

[<Struct>]
type PropertyToken(name:string, pos:int option, destr:DestructureKind, align: AlignInfo option, format: string option) =
    member __.Name = name
    member __.Pos = pos
    member __.Destr = destr
    member __.Align = align
    member __.Format = format
    with
        member x.IsPositional with get() = x.Pos.IsSome
        static member Empty = PropertyToken("", None, DestructureKind.Default, None, None)
        override x.ToString() =
            let sb = System.Text.StringBuilder()
            let append (s:string) = sb.Append(s) |> ignore
            append "{"
            if x.Destr <> DestructureKind.Default then
                append (match x.Destr with DestructureKind.Destructure -> "@" | DestructureKind.Stringify -> "$" | _ -> "")
            append (x.Name)
            if x.Align <> None then append ","; append (string x.Align.Value)
            if x.Format <> None then append ":"; append (string x.Format.Value)
            append "}"
            sb.ToString()

type Token =
| Text of startIndex:int * text:string
| Prop of startIndex:int * PropertyToken
    with static member EmptyText = Text(emptyTokenData)
            static member EmptyProp = Prop(0, PropertyToken.Empty)
            override x.ToString() = match x with
                                    | Text (_, s) -> s
                                    | Prop (_, pd) -> (string pd)

type Template = { FormatString: string; Tokens: Token list }
    with member this.GetProperties() = 
            let choosePropertyTokens t = match t with Token.Prop (_, pd) -> Some pd | _ -> None
            this.Tokens |> Seq.choose choosePropertyTokens |> Seq.toArray

/// A simple value type.
type Scalar =
| [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>] Null
| Bool of bool | Char of char | Byte of byte
| Int16 of int16 | UInt16 of uint16
| Int32 of int32 | UInt32 of uint32
| Int64 of int64 | UInt64 of uint64
| Single of single | Double of double
| Decimal of decimal | String of string
| DateTime of System.DateTime | DateTimeOffset of System.DateTimeOffset
| TimeSpan of System.TimeSpan | Guid of System.Guid | Uri of System.Uri
| Other of obj
with member x.GetValueAsObject() =
        match x with
        | Null -> null
        | Bool b -> box b
        | Char c -> box c
        | Byte b -> box b
        | Int16 i -> box i | UInt16 i -> box i
        | Int32 i -> box i | UInt32 i -> box i
        | Int64 i -> box i | UInt64 i -> box i
        | Single i -> box i | Double i -> box i
        | Decimal i -> box i | String i -> box i
        | DateTime i -> box i | DateTimeOffset i -> box i
        | TimeSpan i -> box i | Guid i -> box i | Uri i -> box i
        | Other i -> i

type ScalarKeyValuePair = Scalar * TemplatePropertyValue
and PropertyNameAndValue = string * TemplatePropertyValue
and TemplatePropertyValue =
| ScalarValue of Scalar
| SequenceValue of TemplatePropertyValue seq
| StructureValue of typeTag:string option * values:PropertyNameAndValue list
| DictionaryValue of data: ScalarKeyValuePair seq

type Destructurer = DestructureRequest -> TemplatePropertyValue option
and DestructureRequest = {
    Kind: DestructureKind
    Value: obj
    It: Destructurer }

let getDestrFromChar = function
                       | '@' -> DestructureKind.Destructure
                       | '$' -> DestructureKind.Stringify
                       | _ -> DestructureKind.Default

let tryParseInt32 s = System.Int32.TryParse(s, System.Globalization.NumberStyles.None, System.Globalization.CultureInfo.InvariantCulture)
let maybeInt32 s =
    if System.String.IsNullOrEmpty(s) then None
    else match tryParseInt32 s with true, n -> Some n | false, _ -> None

let getSb =
    let sb = Text.StringBuilder()
    fun () -> sb.Length <- 0;  sb

let parseTextToken (startAt:int) (template:string) : int*Token =
    let chars = template
    let tlen = chars.Length
    let sb = getSb()
    let inline append (ch:char) = sb.Append(ch) |> ignore
    let rec go i =
        if i >= tlen then tlen, Text(startAt, sb.ToString())
        else
            let c = chars.[i]
            match c with
            | '{' ->
                if (i+1) < tlen && chars.[i+1] = '{' then append c; go (i+2) (*c::acc*) // treat "{{" and a single "{"
                else
                    // start of a property, bail out here
                    if i = startAt then startAt, Token.EmptyText
                    else i, Text(startAt, sb.ToString())
            | '}' when (i+1) < tlen && chars.[i+1] = '}' -> // treat "}}" as a single "}"
                append c
                go (i+2) (*c::acc*) 
            | _ ->
                // append this char and keep going
                append c
                go (i+1) (*c::acc*)
    go startAt

let inline isLetterOrDigit c = System.Char.IsLetterOrDigit(c)
let inline isValidInPropName c = c = '_' || isLetterOrDigit c
let inline isValidInDestrHint c = c = '@' || c = '$'
let inline isValidInAlignment c = c = '-' || System.Char.IsDigit(c)
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

let tryGetFirstCharRng predicate (s:string) (rng:Range) =
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
let rngIndexOf (s:string) (rng:Range) (c:char) : int =
    match tryGetFirstCharRng ((=) c) s rng with None -> -1 | Some i -> i

let tryParseAlignInfoRng (s:string) (rng:Range option) : bool * AlignInfo option =
    match s, rng with
    | _, None -> true, None
    | s, Some rng when (rng.Start >= rng.End) || (hasAnyInvalidRng isValidInAlignment s rng) -> false, None
    | s, Some rng ->
        // todo: no substring here? try the fsharp/fsharp/format.fs way of parsing the number?
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

let tryGetPropInSubString (t:string) (within : Range) : Token option =
    
    let rngIdxWithin = rngIndexOf t within

    /// Given a template such has "Hello, {@name,-10:abc}!" and a *within* range
    /// of Start=8, End=19 (i.e. the full 'insides' of the property tag between { and },
    /// this returns the Start/End pairs of the Name, Align, and Format components. If
    /// anything 'invalid' is found then the first Range is a value of None.
    let nameRange, alignRange, formatRange =
        match rngIdxWithin ',', rngIdxWithin ':' with
        // neither align nor format
        | -1, -1 -> Some within, None, None
        // has format part, but does not have align part
        | -1, fmtIdx -> Some (Range(within.Start, fmtIdx-1)), None, Some (Range(fmtIdx+1, within.End))
        // has align part, but does not have format part
        | alIdx, -1 -> Some (Range(within.Start, alIdx-1)), Some (Range(alIdx+1, within.End)), None
        | alIdx, fmtIdx when alIdx < fmtIdx && alIdx <> (fmtIdx - 1) -> // has both parts in correct order
            let align = Some (Range(alIdx+1, fmtIdx-1))
            let fmt = Some (Range(fmtIdx+1, within.End))
            Some (Range(within.Start, alIdx-1)), align, fmt
        // has format part, no align (but one or more commas *inside* the format string)
        | alIdx, fmtIdx when alIdx > fmtIdx ->
            Some (Range(within.Start, fmtIdx-1)), None, Some (Range(fmtIdx+1, within.End))
        | _, _ -> None, None, None // hammer time; you can't split this

    match nameRange, alignRange, formatRange with
    | None, _, _ -> None
    | Some nameAndDestr, _, _ ->
        let destr = getDestrFromChar (t.[nameAndDestr.Start])
        let propertyName = match destr with
                           | DestructureKind.Default -> nameAndDestr.GetSubString t
                           | _ -> Range(nameAndDestr.Start+1, nameAndDestr.End).GetSubString t

        if propertyName = "" || (hasAnyInvalid isValidInPropName propertyName) then None
        elif formatRange.IsSome && (hasAnyInvalidRng isValidInFormat t formatRange.Value) then None
        else match (tryParseAlignInfoRng t alignRange) with
             | false, _ -> None
             | true, alignInfo ->
                let format = formatRange |> Option.map (fun rng -> rng.GetSubString t)
                Some (Prop(within.Start - 1, 
                           PropertyToken(propertyName, maybeInt32 propertyName, destr, alignInfo, format)))

let parsePropertyToken (startAt:int) (messageTemplate:string) : int*Token =
    let tlen = messageTemplate.Length
    let inline getc idx = messageTemplate.[idx]
    let first = startAt

    // skip over characters after the open-brace, until we reach a character that
    // is *NOT* a valid part of the property tag. this will be the close brace character
    // if the template is actually a well-formed property tag.
    let tryGetFirstInvalidInProp = tryGetFirstChar (not << isValidCharInPropTag)
    let nextInvalidCharIndex =
        match tryGetFirstInvalidInProp messageTemplate (first+1) with
        | Some idx -> idx
        | None -> tlen

    // if we stopped at the end of the string or the last char wasn't a close brace
    // then we treat all the characters we found as a text token, and finish.
    if nextInvalidCharIndex = tlen || getc nextInvalidCharIndex <> '}' then
        nextInvalidCharIndex, Token.Text(first, messageTemplate.Substring(first, nextInvalidCharIndex - first))
    else
        // skip over the trailing "}" close prop tag
        let nextIndex = nextInvalidCharIndex + 1
        let propInsidesRng = Range(first + 1, nextIndex - 2)
        match tryGetPropInSubString messageTemplate propInsidesRng with
        | Some p -> nextIndex, p
        | None _ -> nextIndex, Token.Text(first, (messageTemplate.Substring(first, nextIndex - first)))

let emptyTextTokenList = [ Token.EmptyText ]

let parseTokens (template:string) : Token list = 
    if System.String.IsNullOrEmpty(template) then emptyTextTokenList
    else
        let tlen =  template.Length
        let rec go start acc =
            if start >= tlen then List.rev acc
            else
                match parseTextToken start template with
                | next, tok when next <> start -> // a token was parsed
                    go next (tok::acc)
                | next, _ when next = start -> // no token parsed
                    match parsePropertyToken start template with
                    | nextFromProp, tok when nextFromProp <> start -> // prop was parsed
                        go nextFromProp (tok::acc)
                    | nextFromProp, _ when nextFromProp = start -> // no token parsed
                        List.rev acc // we are done here
                    | _, _ -> failwith "this is not possible - just keep F# compiler happy"
                | _, _ -> failwith "this is not possible - just keep F# compiler happy"
        
        go 0 []

let parse (s:string) = { FormatString = s; Tokens = s |> parseTokens }


open System
type DtOffset = System.DateTimeOffset

let private tryCastAs<'T> (o:obj) =  match o with | :? 'T as res -> Some res | _ -> None
type private TypeScalarFactoryTuple = Type * (obj->Scalar option)
let private builtInScalarTypeFactories : TypeScalarFactoryTuple list =
    [
            typedefof<bool>,       function :? bool as v -> Some (Scalar.Bool v)               | _ -> None
            typedefof<char>,       function :? char as v -> Some (Scalar.Char v)               | _ -> None
            typedefof<byte>,       function :? byte as v -> Some (Scalar.Byte v)               | _ -> None
            typedefof<int16>,      function :? int16 as v -> Some (Scalar.Int16 v)             | _ -> None
            typedefof<uint16>,     function :? uint16 as v -> Some (Scalar.UInt16 v)           | _ -> None
            typedefof<int32>,      function :? int as v -> Some (Scalar.Int32 v)               | _ -> None
            typedefof<uint32>,     function :? uint32 as v -> Some (Scalar.UInt32 v)           | _ -> None
            typedefof<int64>,      function :? int64 as v -> Some (Scalar.Int64 v)             | _ -> None
            typedefof<uint64>,     function :? uint64 as v -> Some (Scalar.UInt64 v)           | _ -> None
            typedefof<single>,     function :? single as v -> Some (Scalar.Single v)           | _ -> None
            typedefof<double>,     function :? double as v -> Some (Scalar.Double v)           | _ -> None
            typedefof<decimal>,    function :? decimal as v -> Some (Scalar.Decimal v)         | _ -> None
            typedefof<string>,     function :? string as v -> Some (Scalar.String v)           | _ -> None
            typedefof<DateTime>,   function :? DateTime as v -> Some (Scalar.DateTime v)       | _ -> None
            typedefof<DtOffset>,   function :? DtOffset as v -> Some (Scalar.DateTimeOffset v) | _ -> None
            typedefof<TimeSpan>,   function :? TimeSpan as v -> Some (Scalar.TimeSpan v)       | _ -> None
            typedefof<Guid>,       function :? Guid as v -> Some (Scalar.Guid v)               | _ -> None
            typedefof<Uri>,        function :? Uri as v -> Some (Scalar.Uri v)                 | _ -> None
    ]

/// Given a Type, creates a tuple with the type itself, and a function that can convert
/// an object to a Scalar of the given custom type.
let private toCustomTypeScalarFactoryTuple (t:Type) : TypeScalarFactoryTuple =
    let getIfTypeMatches (v:obj) = match v.GetType() with
                                    | theType when theType.Equals(t) -> Some (Scalar.Other v)
                                    | _ -> None
    t, getIfTypeMatches
    
/// Creates a <see cref="Destructurer" /> which combines the built-in scalar types
/// with the provided 'otherScalarTypes' sequence.
let private createScalarDestr (typesAndFactories: TypeScalarFactoryTuple list) : Destructurer =
    let typeToScalarFactoryDict = typesAndFactories |> dict
    fun (r:DestructureRequest) ->
        match typeToScalarFactoryDict.TryGetValue (r.Value.GetType()) with
        | true, factory -> match (factory r.Value) with
                            | Some s -> Some (ScalarValue s)
                            | None -> None
        | _, _ -> None

let tryBuiltInTypes = createScalarDestr builtInScalarTypeFactories

let tryNullable (r:DestructureRequest) =
    let t = r.Value.GetType()
    let isNullable = t.IsConstructedGenericType && t.GetGenericTypeDefinition() = typeof<Nullable<_>>
    if isNullable then
        match tryCastAs<System.Nullable<_>>() with
        | Some n when n.HasValue -> r.It { r with Value=box (n.GetValueOrDefault()) } // re-destructure with the value inside
        | Some n when (not n.HasValue) -> Some (ScalarValue (Scalar.Null))
        | _ -> None
    else
        None

let tryEnum (r:DestructureRequest) = 
    match tryCastAs<System.Enum>(r.Value) with
    | Some e -> Some (ScalarValue (Scalar.Other e))
    | None -> None

let tryByteArrayMaxBytes (maxBytes:int) (r:DestructureRequest) =
    match tryCastAs<System.Byte[]>(r.Value) with
    | Some bytes when bytes.Length <= maxBytes -> Some (ScalarValue (Scalar.Other(bytes)))
    | Some bytes when bytes.Length > maxBytes ->
        let toHexString (b:byte) = b.ToString("X2")
        let start = bytes |> Seq.take maxBytes |> Seq.map toHexString |> String.Concat
        let description = start + "... (" + string bytes.Length + " bytes)"
        Some (ScalarValue (Scalar.Other(description)))
    | _ -> None

let tryByteArray = tryByteArrayMaxBytes 1024

let tryReflectionTypes (r:DestructureRequest) =
    match r.Value with
    | :? Type as t -> Some (ScalarValue (Scalar.Other t))
    | :? System.Reflection.MemberInfo as m -> Some (ScalarValue (Scalar.Other m))
    | _ -> None
    
let tryScalarDestructure (r:DestructureRequest) =
    [ tryBuiltInTypes; tryNullable; tryEnum; tryByteArray; tryReflectionTypes ]
    |> List.tryPick (fun tryDestr -> tryDestr r)

type SelfLogger = (string * obj[]) -> unit
let nullLogger (format: string, values: obj[]) = ()

let constructPosOrNamed (log: SelfLogger)
                        (constructPositional)
                        (constructNamed)
                        (t:Template)
                        (values: obj[])
                        : PropertyNameAndValue seq = 
    // log("Required properties not provided for: {0}", [|box t|])
    let props = t.GetProperties()
    let construct = if props.Length > 0 && props.[0].IsPositional then constructPositional
                    else constructNamed
    construct log t props values

/// 'Zips' the propertes and values the destructure each value, only returning
/// those that were destructured.
let zipDestr (destr:Destructurer) (props:PropertyToken[]) values =
    props
    |> Seq.zip values
    |> Seq.choose (fun (v, pt) ->
        match destr { Kind=pt.Destr; Value=v; It=destr } with
        | Some tpv -> Some (pt.Name, tpv)
        | _ -> None
    )
    |> Seq.cache

let constructPositional (destr: Destructurer)
                        (log: SelfLogger)
                        (t: Template)
                        (positionals: PropertyToken[])
                        (values: obj[]) : PropertyNameAndValue seq =
    match positionals with
    | null -> Seq.empty
    | _ -> 
        if positionals.Length <> values.Length then
            log("Positional property count does not match parameter count: {0}", [|box t|])
        zipDestr destr positionals values

let constructNamed (destr: Destructurer)
                   (log: SelfLogger)
                   (t: Template)
                   (named: PropertyToken[])
                   (values: obj[]) : PropertyNameAndValue seq =
    match named with
    | null -> Seq.empty
    | _ -> 
        if named.Length <> values.Length then
            log("Named property count does not match parameter count: {0}", [|box t|])
        zipDestr destr named values

let tryNull (r:DestructureRequest) =
    match r.Value with
    | null -> Some (ScalarValue Scalar.Null)
    | _ -> None

let tryStringifyDestructurer (r:DestructureRequest) =
    match r with
    | { Kind=DestructureKind.Stringify; Value=null } -> Some (ScalarValue Scalar.Null)
    | { Kind=DestructureKind.Stringify } -> Some (ScalarValue (Scalar.String (r.Value.ToString())))
    | _ -> None

let tryDestructuringDestr (r:DestructureRequest)  = None // TODO:
let tryEnumerableDestr (r:DestructureRequest) = None // TODO:

let scalarStringCatchAllDestr (r:DestructureRequest) = Some (ScalarValue (Scalar.String (r.Value.ToString())))

let destrsInOrder = [tryNull; tryStringifyDestructurer;
                     tryScalarDestructure; tryDestructuringDestr;
                     tryEnumerableDestr; scalarStringCatchAllDestr ]

let destr r = destrsInOrder |> List.tryPick (fun d -> d r)

let capturePropertiesWith (d:Destructurer) (t:Template) (args: obj[]) =
    constructPosOrNamed nullLogger (constructPositional d) (constructNamed d) t args

let captureProperties (t:Template) (args:obj[]) = capturePropertiesWith destr t args

let captureMessageProperties (s:string) (args:obj[]) = s |> parse |> (fun templ -> captureProperties templ args)

let writePropToken (tw: IO.TextWriter)
                   (pt: PropertyToken)
                   (pv: TemplatePropertyValue) =
    let ptFormatString = pt.ToString()
    match pv with
    | ScalarValue (Scalar.Null) -> tw.Write(format=ptFormatString, arg=[|null|])
    | _ -> failwithf "Not yet implemented value %A" pv

/// Converts template token and value into a rendered string.
let writeToken (tw: IO.TextWriter)
               (writePropToken)
               (tokenAndPropValue: Token * TemplatePropertyValue option) =
    match tokenAndPropValue with
    | Token.Text (_, raw), None -> tw.Write(raw) |> ignore
    | Token.Prop (_, pt), Some pv -> writePropToken tw pt pv
    | Token.Prop (_, pt), None -> failwithf "unexpected property token %A and no associated value" pt
    | Token.Text (_, raw), Some pv -> failwithf "unexpected text token %s with property value %A" raw pv

/// Defines the signature of a method that matches property tokens to values
type TokenPropertyValueMatcher = (Token list * obj[]) -> (Token * TemplatePropertyValue option) seq

let defaultTokenPropertyValueMatcher : TokenPropertyValueMatcher =
    (fun tokensAndObjs -> Seq.empty) // TODO

let doFormat   (provider:IFormatProvider)
               (matchTokensToPropertyValues: TokenPropertyValueMatcher)
               (writePropToken)
               (template:Template)
               (values:obj[]) =

    use tw = new IO.StringWriter(provider)
    (matchTokensToPropertyValues (template.Tokens, values)) |> Seq.iter (writeToken tw writePropToken)
    tw.ToString()

let format provider template values =
    doFormat provider defaultTokenPropertyValueMatcher writePropToken template values

let bprintn (sb:System.Text.StringBuilder) (template:string) (args:obj[]) = () // TODO:
let sfprintn (p:System.IFormatProvider) (template:string) (args:obj[]) = "" // TODO:
let fprintn (tw:System.IO.TextWriter) (template:string) (args:obj[]) = () // TODO:
