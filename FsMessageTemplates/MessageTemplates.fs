namespace FsMessageTemplates

open System.Text
open System.IO

type DestrHint = Default = 0 | Stringify = 1 | Destructure = 2
type Direction = Left = 0 | Right = 1

[<AutoOpen>]
module Extensions =

    type DestrHint with
        member inline this.ToDestrChar () = if this = DestrHint.Default then ""
                                                elif this = DestrHint.Destructure then "@"
                                                else "$"
        static member inline FromChar c =   if c = '@' then DestrHint.Destructure
                                                elif c = '$' then DestrHint.Stringify
                                                else DestrHint.Default

    type System.Text.StringBuilder with
        /// Appends the text if the condition is true, returning the string builer for chaining.
        member inline this.AppendIf (cond:bool, s:string) = if cond then this.Append(s) else this
        /// Assists with reused string builders
        member inline this.ToStringAndClear () = let s = this.ToString() in this.Clear() |> ignore; s

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

[<Struct>]
type PropertyToken(name:string, pos:int, destr:DestrHint, align: AlignInfo, format: string) =
    static member Empty = PropertyToken("", -1, DestrHint.Default, AlignInfo.Empty, null)
    member __.Name = name
    member __.Pos = pos
    member __.Destr = destr
    member __.Align = align
    member __.Format = format
    member x.IsPositional with get() = x.Pos >= 0
    member private x.AppendPropertyString (sb:StringBuilder, includeDestr:bool, name:string) =
        sb  .Append("{")
            .AppendIf(includeDestr && x.Destr <> DestrHint.Default, x.Destr.ToDestrChar())
            .Append(name)
            .AppendIf(not x.Align.IsEmpty, "," + ((if x.Align.Direction = Direction.Left then "-" else "") + string x.Align.Width))
            .AppendIf(x.Format <> null, ":" + x.Format)
            .Append("}")
    override x.ToString() = x.AppendPropertyString(StringBuilder(), true, name).ToString()

type Token =
| Text of startIndex:int * text:string
| Prop of startIndex:int * PropertyToken
    override x.ToString() = match x with | Text (_, s) -> s | Prop (_, pd) -> (string pd)

type Template(formatString:string, tokens: Token[], isNamed:bool, properties:PropertyToken[]) =
    member this.Tokens = tokens :> Token seq
    member this.FormatString = formatString
    member this.Properties = properties :> PropertyToken seq
    member internal this.Named = if isNamed then properties else Unchecked.defaultof<PropertyToken[]>
    member internal this.PositionalsByPos = if isNamed then Unchecked.defaultof<PropertyToken[]> else properties
    member internal this.HasAnyProperties = properties.Length > 0

type ScalarKeyValuePair = TemplatePropertyValue * TemplatePropertyValue
and PropertyNameAndValue = { Name:string; Value:TemplatePropertyValue }
and TemplatePropertyValue =
| ScalarValue of obj
| SequenceValue of TemplatePropertyValue list
| StructureValue of typeTag:string * values:PropertyNameAndValue list
| DictionaryValue of data: ScalarKeyValuePair list
    static member ScalarNull = ScalarValue null
    static member Empty = Unchecked.defaultof<TemplatePropertyValue>

[<AutoOpen>]
module Log =
    /// Describes a function for logging warning messages.
    type SelfLogger = (string * obj[]) -> unit

    /// An inline null logger; for ignoring any warning messages.
    let inline nullLogger (_: string, _: obj[]) = ()

module Defaults =
    let maxDepth = 10

type Destructurer = DestructureRequest -> TemplatePropertyValue
and
    [<NoEquality; NoComparison>]
    DestructureRequest(destructurer:Destructurer, value:obj, ?maxDepth:int, ?currentDepth:int, ?hint:DestrHint) =
        member x.Hint = defaultArg hint DestrHint.Default
        member x.Value = value
        member x.Destructurer = destructurer
        member x.MaxDepth = defaultArg maxDepth 10
        member x.CurrentDepth = defaultArg currentDepth 1
        member internal x.Log = nullLogger
        /// During destructuring, this is called to 'recursively' destructure child properties
        /// or sequence elements.
        member inline internal x.TryAgainWithValue (newValue:obj, ?newDestructurer: Destructurer) =
            let nextDepth = x.CurrentDepth + 1
            if nextDepth > x.MaxDepth then TemplatePropertyValue.ScalarNull
            else 
                let destructurer = defaultArg newDestructurer x.Destructurer
                let childRequest = DestructureRequest(destructurer, newValue, x.MaxDepth, nextDepth, x.Hint)
                // now invoke the full destructurer again on the new value
                x.Destructurer childRequest

[<RequireQualifiedAccess>]
module Empty =
    let textToken = Text(0, "")
    let propToken = Prop(0, PropertyToken.Empty)
    let textTokenArray = [| textToken |]

module Parser =
    /// Parses a text token inside the template string, starting at a specified character number within the
    /// template string, and returning the 'next' character index + the parsed text token. The StringBuilder
    /// is used as a temporary buffer for collecting the text token characters, and is cleared before returning.
    /// The text token is 'finished' when an open brace is encountered (or the end of the template string is
    /// reached (whichever comes first).
    let inline parseTextToken (sb:StringBuilder) (startAt:int) (template:string) : int*Token =
        let chars = template
        let tlen = chars.Length
        let inline append (ch:char) = sb.Append(ch) |> ignore
        let rec go i =
            if i >= tlen then tlen, Text(startAt, sb.ToStringAndClear())
            else
                let c = chars.[i]
                match c with
                | '{' -> if (i+1) < tlen && chars.[i+1] = '{' then append c; go (i+2)
                         elif i = startAt then startAt, Empty.textToken
                         else i, Text(startAt, sb.ToStringAndClear())
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
        member inline this.Start = startIndex
        member inline this.End = endIndex
        member inline this.Length = (endIndex - startIndex) + 1
        member inline this.GetSubString (s:string) = s.Substring(startIndex, this.Length)
        member inline this.IncreaseBy startNum endNum = Range(startIndex+startNum, endIndex+endNum)
        member inline this.Right (startFromIndex:int) =
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

    /// Attemps to parse an integer from the range within a string. Returns Int32.MinValue if the string does
    /// not contain an integer. The minus sign '-' is allowed only as the first character.
    let inline tryParseIntFromRng (invalidValue:int) (s:string) (rng:Range) =
        if rng.Length = 1 && '0' <= s.[0] && s.[0] <= '9' then
            int (s.[rng.Start]) - 48
        else
            let indexOfLastCharPlus1 = rng.End+1
            let rec inside isNeg numSoFar i =
                if i = indexOfLastCharPlus1 then
                    if isNeg then -numSoFar else numSoFar
                else
                    let c = s.[i]
                    if c = '-' then
                        if i = rng.Start then inside true (numSoFar) (i+1)
                        else invalidValue // no '-' character allowed other than first char
                    elif '0' <= c && c <= '9' then
                        inside isNeg (10*numSoFar + int c - 48) (i+1)
                    else invalidValue

            inside false 0 rng.Start

    /// Attemps to parse an integer from the string. Returns -1 if the string does
    /// not contain an integer.
    let inline parseIntOrNegative1 s =
        if System.String.IsNullOrEmpty(s) then -1
        else tryParseIntFromRng -1 s (Range(0, s.Length-1))

    let inline tryParseAlignInfoRng (s:string) (rng:Range option) : AlignInfo =
        match s, rng with
        | _, None -> AlignInfo(isValid=true)
        | s, Some rng when (rng.Start > rng.End) || (hasAnyInvalidRng isValidInAlignment s rng) ->
            AlignInfo(isValid=false)
        | s, Some rng ->
            let invalidAlignWidth = System.Int32.MinValue
            let width = 
                match tryParseIntFromRng invalidAlignWidth s rng with
                | System.Int32.MinValue -> 0 // not a valid align number (e.g. dash in wrong spot)
                | n -> n
            if width = 0 then AlignInfo(isValid=false)
            else
                let isNegativeAlignWidth = width < 0
                let direction = if isNegativeAlignWidth then Direction.Left else Direction.Right
                AlignInfo(direction, abs(width))

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
            | alIdx, fmtIdx when alIdx < fmtIdx && alIdx <> (fmtIdx-1) -> // has both parts in correct order
                let align = Some (Range(alIdx+1, fmtIdx-1))
                let fmt = Some (Range(fmtIdx+1, within.End))
                Some (Range(within.Start, alIdx-1)), align, fmt
            | alIdx, fmtIdx when alIdx > fmtIdx ->
                Some (Range(within.Start, fmtIdx-1)), None, Some (Range(fmtIdx+1, within.End)) // has format part, no align (but one or more commas *inside* the format string)
            | _, _ -> None, None, None // hammer time; you can't split this

        match nameRange, alignRange, formatRange with
        | None, _, _ -> Empty.propToken
        | Some nameAndDestr, _, _ ->
            let destr = DestrHint.FromChar t.[nameAndDestr.Start]
            let propertyName = match destr with
                               | DestrHint.Default -> nameAndDestr.GetSubString t
                               | _ -> Range(nameAndDestr.Start+1, nameAndDestr.End).GetSubString t
            if propertyName = "" || (hasAnyInvalid isValidInPropName propertyName) then Empty.propToken
            elif formatRange.IsSome && (hasAnyInvalidRng isValidInFormat t formatRange.Value) then Empty.propToken
            else match (tryParseAlignInfoRng t alignRange) with
                 | ai when ai.IsValid = false -> Empty.propToken
                 | alignInfo ->
                    let format = if formatRange.IsSome then (formatRange.Value.GetSubString t) else null
                    Prop(within.Start - 1,
                         PropertyToken(propertyName, parseIntOrNegative1 propertyName, destr, alignInfo, format))

    let parseTokens (mt:string) =
        let tlen = mt.Length
        if tlen = 0 then Empty.textTokenArray
        else
            let sb = StringBuilder()
            let rz = ResizeArray<Token>()
            let rec go start =
                if start >= tlen then rz.ToArray()
                else match parseTextToken sb start mt with
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
                            | p when obj.ReferenceEquals(p, Empty.propToken) ->
                                rz.Add (Token.Text(first, (mt.Substring(first, nextIndex - first))))
                                go nextIndex
                            | p -> rz.Add p; go nextIndex
            go 0

    let parse (s:string) =
        let tokens = s |> parseTokens
        let mutable allPos, anyPos = true, false
        let rzProps = ResizeArray<PropertyToken>()
        for i = 0 to (tokens.Length-1) do
            let t = tokens.[i]
            match t with
            | Prop (_, pt) ->
                rzProps.Add pt
                if pt.IsPositional then anyPos <- true else allPos <- false
            | _ -> ()
        let properties = rzProps.ToArray()
        if allPos then properties |> Array.sortInPlaceBy (fun p -> p.Pos)
        Template(s, tokens, not allPos, properties)

module Destructure =
    open System

    // perf
    let inline isEmptyKeepTrying (tpv:TemplatePropertyValue) = Object.ReferenceEquals(tpv, null)
    let inline tryCastAs<'T> (o:obj) =  match o with | :? 'T as res -> res | _ -> Unchecked.defaultof<'T>
    
    let scalarTypes = 
        [ typeof<bool>;      typeof<char>;       typeof<byte>;              typeof<int16>
          typeof<uint16>;    typeof<int32>;      typeof<uint32>;            typeof<int64>
          typeof<uint64>;    typeof<single>;     typeof<double>;            typeof<decimal>
          typeof<string>;    typeof<DateTime>;   typeof<DateTimeOffset>;    typeof<TimeSpan>
          typeof<Guid>;      typeof<Uri>; ]

    let scalarTypeHash = System.Collections.Generic.HashSet(scalarTypes)

    let inline tryBuiltInTypesOrNull (r:DestructureRequest) =
        if r.Value = null then ScalarValue null 
        elif scalarTypeHash.Contains(r.Value.GetType()) then (ScalarValue r.Value)
        else TemplatePropertyValue.Empty

    let inline tryNullable (r:DestructureRequest) =
        let t = r.Value.GetType()
        let isNullable = t.IsConstructedGenericType && t.GetGenericTypeDefinition() = typedefof<Nullable<_>>
        if isNullable then
            match tryCastAs<System.Nullable<_>>(r.Value) with
            | n when (Object.ReferenceEquals(null, n)) -> (ScalarValue null)
            | n when (not n.HasValue) -> (ScalarValue null)
            | n when n.HasValue -> r.TryAgainWithValue(box (n.GetValueOrDefault()))
            | _ -> TemplatePropertyValue.Empty
        else TemplatePropertyValue.Empty

    let inline tryEnum (r:DestructureRequest) =
        match tryCastAs<System.Enum>(r.Value) with
        | e when (Object.ReferenceEquals(null, e)) -> TemplatePropertyValue.Empty
        | e -> (ScalarValue (e))

    let inline tryByteArrayMaxBytes (maxBytes:int) (r:DestructureRequest) =
        match tryCastAs<System.Byte[]>(r.Value) with
        | bytes when (Object.ReferenceEquals(null, bytes)) -> TemplatePropertyValue.Empty
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
        | _ -> TemplatePropertyValue.Empty

    let inline tryScalarDestructure (r:DestructureRequest) =
        match tryBuiltInTypesOrNull r with
        | ekt1 when isEmptyKeepTrying ekt1 ->
            match tryNullable r with
            | ekt2 when isEmptyKeepTrying ekt2 ->
                match tryEnum r with
                | ekt3 when isEmptyKeepTrying ekt3 ->
                    match tryByteArray r with
                    | ekt4 when isEmptyKeepTrying ekt4 ->
                        match tryReflectionTypes r with
                        | ekt5 when isEmptyKeepTrying ekt5 -> TemplatePropertyValue.Empty
                        | tpv -> tpv
                    | tpv -> tpv
                | tpv -> tpv
            | tpv -> tpv
        | tpv -> tpv


    let inline tryNull (r:DestructureRequest) =
        match r.Value with | null -> ScalarValue null | _ -> TemplatePropertyValue.Empty
    let inline tryStringifyDestructurer (r:DestructureRequest) =
        match r.Hint with | DestrHint.Stringify -> ScalarValue (r.Value.ToString()) | _ -> TemplatePropertyValue.Empty

    let inline tryDelegateString (r:DestructureRequest) =
        if r.Hint <> DestrHint.Destructure then TemplatePropertyValue.Empty
        else
            match tryCastAs<System.Delegate>(r.Value) with
            | e when (Object.ReferenceEquals(null, e)) -> TemplatePropertyValue.Empty
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
        | e when Object.ReferenceEquals(null, e) -> TemplatePropertyValue.Empty
        | e when isScalarDict valueType ->
            let mutable keyProp, valueProp = Unchecked.defaultof<PropertyInfo>, Unchecked.defaultof<PropertyInfo>
            let getKey o = if keyProp = null then keyProp <- o.GetType().GetRuntimeProperty("Key")
                           keyProp.GetValue(o)
            let getValue o = if valueProp = null then valueProp <- o.GetType().GetRuntimeProperty("Value")
                             valueProp.GetValue(o)
            let skvps = e |> Seq.cast<obj>
                          |> Seq.map (fun o -> getKey o, getValue o)
                          |> Seq.map (fun (key, value) ->
                            // only attempt the built-in scalars for the keyValue, because only
                            // scalar values are supported as dictionary keys. However, do full 
                            // destructuring for the dictionary entry values.
                            r.TryAgainWithValue (key, tryScalarDestructure), r.TryAgainWithValue (value))
                          |> Seq.toList
            DictionaryValue skvps
        | e -> SequenceValue(e |> Seq.cast<obj> |> Seq.map r.TryAgainWithValue |> Seq.toList)

    let inline scalarStringCatchAllDestr (r:DestructureRequest) = ScalarValue (r.Value.ToString())

    let inline isPublicInstanceReadProp (p:PropertyInfo) =
        p.CanRead && p.GetMethod.IsPublic && not (p.GetMethod.IsStatic) &&
            (p.Name <> "Item" || p.GetIndexParameters().Length = 0)

    let inline tryObjectStructureDestructuring (r:DestructureRequest) =
        if r.Hint <> DestrHint.Destructure then TemplatePropertyValue.Empty
        else
            let ty = r.Value.GetType()
            let typeTag = match ty.Name with
                          | s when s.Length = 0 || not (Char.IsLetter s.[0]) -> null
                          | s -> s
            
            let rzPubProps = ResizeArray<PropertyInfo>()
            for rtp in ty.GetRuntimeProperties() do
                if isPublicInstanceReadProp rtp then rzPubProps.Add rtp

            // Recursively destructure the child properties
            let rec loopDestrChildren i acc = 
                if i < 0 then acc
                else
                    let pi = rzPubProps.[i]
                    try
                        let propValue = pi.GetValue(r.Value)
                        let propTpv = { Name=pi.Name; Value=r.TryAgainWithValue propValue }
                        loopDestrChildren (i-1) (propTpv :: acc)
                    with
                        | :? TargetParameterCountException as ex ->
                            r.Log("The property accessor {0} is a non-default indexer", [|pi|])
                            loopDestrChildren (i-1) (acc)
                        | :? TargetInvocationException as ex ->
                            r.Log("The property accessor {0} threw exception {1}", [| pi; ex; |])
                            let propValue = "The property accessor threw an exception:" + ex.InnerException.GetType().Name
                            let propTpv = { Name=pi.Name; Value=r.TryAgainWithValue propValue }
                            loopDestrChildren (i-1) (propTpv :: acc)

            let childStructureValues = loopDestrChildren (rzPubProps.Count-1) []
            StructureValue(typeTag, childStructureValues)

    /// A destructurer that does nothing by returning TemplatePropertyValue.Empty
    let inline alwaysKeepTrying (_:DestructureRequest) = TemplatePropertyValue.Empty

    /// Attempts all built-in destructurers in the correct order, falling
    /// back to 'scalarStringCatchAllDestr' (stringify) if no better option
    /// is found first. Also supports custom scalars and custom object
    /// destructuring at the appropriate points in the pipline. Note this is
    /// called recursively in a tight loop during the process of capturing
    /// template property values, which means it needs to be fairly fast.
    let inline tryAllWithCustom (tryCustomScalarTypes: Destructurer option)
                                (tryCustomDestructure: Destructurer option)
                                (request: DestructureRequest) =
        let tryCustomDestructure = defaultArg tryCustomDestructure alwaysKeepTrying
        let tryCustomScalarTypes = defaultArg tryCustomScalarTypes alwaysKeepTrying

        // Performance :(
        match tryNull request with
        | tpv when isEmptyKeepTrying tpv ->
            match tryStringifyDestructurer request with
            | tpv when isEmptyKeepTrying tpv ->
                match tryScalarDestructure request with
                | tpv when isEmptyKeepTrying tpv ->
                    match tryCustomScalarTypes request with
                    | tpv when isEmptyKeepTrying tpv ->
                        match tryDelegateString request with
                        | tpv when isEmptyKeepTrying tpv ->
                            match tryCustomDestructure request with
                            | tpv when isEmptyKeepTrying tpv ->
                                match tryEnumerableDestr request with
                                | tpv when isEmptyKeepTrying tpv ->
                                    match tryObjectStructureDestructuring request with
                                    | tpv when isEmptyKeepTrying tpv ->
                                        match scalarStringCatchAllDestr request with
                                        | tpv when isEmptyKeepTrying tpv -> TemplatePropertyValue.Empty
                                        | tpv -> tpv
                                    | tpv -> tpv
                                | tpv -> tpv
                            | tpv -> tpv
                        | tpv -> tpv
                    | tpv -> tpv
                | tpv -> tpv
            | tpv -> tpv
        | tpv -> tpv

module Capturing =
    /// Determines if a TemplatePropertyValue is considered 'empty' (i.e. null) during the
    /// destructuring process. This is to avoid lots of Option<'T> allocations.
    let inline isEmptyKeepTrying (tpv) = Destructure.isEmptyKeepTrying (tpv)

    let createCustomDestructurer (tryScalars:Destructurer option) (tryCustomObjects: Destructurer option) : Destructurer =
        Destructure.tryAllWithCustom tryScalars tryCustomObjects

    let defaultDestructureNoCustoms : Destructurer = Destructure.tryAllWithCustom None None

    /// Captures properties, matching the obj arguments to properties either positionally 
    /// (if all properties are positional) or from left-to-right if one or more are non-positional. The
    /// Destructurer is used to convert the obj arguments into TemplatePropertyValue objects.
    let capturePropertiesWith (log:SelfLogger) (destr:Destructurer) (maxDepth:int) (t:Template) (args: obj[]) =
        if (args = null || args.Length = 0) && t.HasAnyProperties then Array.empty
        elif not t.HasAnyProperties then Array.empty
        else
            let positionalsByPos = t.PositionalsByPos
            let props = if positionalsByPos <> null then positionalsByPos else t.Named
            let mutable matchedRun = props.Length
            if props.Length <> args.Length then
                matchedRun <- min props.Length args.Length
                log("property count does not match parameter count: {0}", [|t.FormatString|])
            let result = Array.zeroCreate<PropertyNameAndValue>(matchedRun)
            for i = 0 to matchedRun-1 do
                let p = props.[i]
                let req = DestructureRequest(destr, args.[i], maxDepth, hint=p.Destr)
                Array.set result i { Name=p.Name; Value=destr req }
            result

    let capturePropertiesCustom (d: Destructurer) (maxDepth: int) (t: Template) (args: obj[]) =
        capturePropertiesWith nullLogger d maxDepth t args
        :> PropertyNameAndValue seq

    let captureProperties (t:Template) (args:obj[]) =
        capturePropertiesWith nullLogger defaultDestructureNoCustoms Defaults.maxDepth t args
        :> PropertyNameAndValue seq

    /// The same as 'captureProperties' except the message template is first
    /// parsed, then the properties are captured.
    let captureMessageProperties (s:string) (args:obj[]) =
        captureProperties (Parser.parse s) args

module Formatting =
    /// Recursively writes the string representation of the template property
    /// value to the provided TextWriter. The provided format string is only
    /// used for a ScalarValue v when v implements System.IFormattable.
    /// TODO: ICustomFormattable?
    let rec writePropValue (w: TextWriter) (pv: TemplatePropertyValue) (format: string) =
        match pv with
        | ScalarValue sv ->
            match sv with
            | null -> w.Write "null"
            | :? string as s ->
                w.Write "\""
                w.Write (s.Replace("\"", "\\\""))
                w.Write "\""
            | :? System.IFormattable as f ->
                w.Write (f.ToString(format, w.FormatProvider))
            | _ -> w.Write(sv.ToString())
        | SequenceValue svs ->
            w.Write '['
            let lastIndex = svs.Length - 1
            svs |> List.iteri (fun i sv ->
                writePropValue w sv null
                if i <> lastIndex then w.Write ", "
            )
            w.Write ']'
        | StructureValue(typeTag, values) ->
            if typeTag <> null then w.Write typeTag; w.Write ' '
            w.Write "{ "
            let lastIndex = values.Length - 1
            for i = 0 to lastIndex do
                let tp = values.[i]
                w.Write tp.Name; w.Write ": "
                writePropValue w tp.Value null
                w.Write (if i = lastIndex then " " else ", ")
            w.Write "}"
        | DictionaryValue(data) ->
            w.Write '['
            data |> List.iter (fun (entryKey, entryValue) ->
                w.Write '('
                writePropValue w entryKey null
                w.Write ": "
                writePropValue w entryValue null
                w.Write ")"
            )
            w.Write ']'

    /// Converts a 'matched' token and template property value into a rendered string.
    /// For properties, the System.String.Format rules are applied, including alignment
    /// and System.IFormattable rules, along with the additional MessageTemplates rules
    /// for named properties and destructure-formatting.
    let inline writeToken (w: TextWriter) (token:Token) (value:TemplatePropertyValue) =
        match token, value with
        | Token.Text (_, raw), _ -> w.Write raw
        | Token.Prop (_, pt), pv ->
            if Destructure.isEmptyKeepTrying pv then w.Write pt // calls ToString on the token
            else
                if pt.Align.IsEmpty then
                    writePropValue w pv pt.Format
                else
                    let alignWriter = new StringWriter(w.FormatProvider)
                    writePropValue alignWriter pv pt.Format
                    let valueAsString = alignWriter.ToString()
                    if valueAsString.Length >= pt.Align.Width then
                        w.Write valueAsString
                    else
                        let pad = pt.Align.Width - valueAsString.Length
                        if pt.Align.Direction = Direction.Right then w.Write (System.String(' ', pad))
                        w.Write valueAsString
                        if pt.Align.Direction = Direction.Left then w.Write (System.String(' ', pad))

    let inline createValuesByPropNameDictionary (values:PropertyNameAndValue seq) =
        System.Linq.Enumerable.ToDictionary(source=values,
                                            keySelector=(fun tp -> tp.Name),
                                            elementSelector=(fun tp -> tp.Value))

    let inline getByName1to5 (values: PropertyNameAndValue[]) (nme: string) =
        let valueCount = values.Length
        if values.[0].Name = nme then values.[0].Value
        elif valueCount > 1 && values.[1].Name = nme then values.[1].Value
        elif valueCount > 2 && values.[2].Name = nme then values.[2].Value
        elif valueCount > 3 && values.[3].Name = nme then values.[3].Value
        elif valueCount > 4 && values.[4].Name = nme then values.[4].Value
        else TemplatePropertyValue.Empty

    let inline getByNameDict (valuesDict: System.Collections.Generic.IDictionary<string, TemplatePropertyValue>) nme =
        let tpv = ref Unchecked.defaultof<TemplatePropertyValue>
        valuesDict.TryGetValue (nme, tpv) |> ignore
        !tpv

    let captureThenFormat (w: TextWriter) (template:Template) (values:obj[]) =
        let values = Capturing.capturePropertiesWith
                        nullLogger Capturing.defaultDestructureNoCustoms Defaults.maxDepth
                        template values
        let valueCount = values.Length
        let getValueForPropName =
            match valueCount with
            | 0 -> fun _ -> TemplatePropertyValue.Empty
            | 1 | 2 | 3 | 4 | 5 -> getByName1to5 values
            | _ -> getByNameDict (createValuesByPropNameDictionary values)

        for t in template.Tokens do
            match t with
            | Token.Text _ as tt -> writeToken w tt Unchecked.defaultof<TemplatePropertyValue>
            | Token.Prop (_, pd) as tp ->
                let value = getValueForPropName pd.Name
                writeToken w tp value

    let format template values =
        use tw = new StringWriter()
        captureThenFormat tw template values
        tw.ToString()

    let formatCustom (t:Template) w getValueByName =
        for tok in t.Tokens do
            match tok with
            | Token.Text _ as tt -> writeToken w tt Unchecked.defaultof<TemplatePropertyValue>
            | Token.Prop (_, pd) as tp ->
                let value = getValueByName pd.Name
                writeToken w tp value

    let bprintsm (sb:StringBuilder) (template:string) (args:obj[]) =
        use tw = new StringWriter(sb)
        captureThenFormat tw (Parser.parse template) args

    let sprintsm (p:System.IFormatProvider) (template:string) (args:obj[]) =
        use tw = new StringWriter(p)
        captureThenFormat tw (Parser.parse template) args
        tw.ToString()

    let fprintsm (tw:TextWriter) (template:string) (args:obj[]) =
        captureThenFormat tw (Parser.parse template) args

    let bprintm (template:Template) (sb:StringBuilder) (args:obj[]) =
        use tw = new StringWriter(sb)
        captureThenFormat tw template args

    let sprintm (template:Template) (provider:System.IFormatProvider) (args:obj[]) =
        use tw = new StringWriter(provider)
        captureThenFormat tw template args
        tw.ToString()

    let fprintm (template:Template) (tw:TextWriter) (args:obj[]) =
        captureThenFormat tw template args
