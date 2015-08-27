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

    let inline tryParseInt32 s = System.Int32.TryParse(s, System.Globalization.NumberStyles.None, System.Globalization.CultureInfo.InvariantCulture)
    let inline parseIntOrNegative1 s =
        if System.String.IsNullOrEmpty(s) then -1
        else match tryParseInt32 s with true, n -> n | _ -> -1

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

    let inline tryParseAlignInfoRng (s:string) (rng:Range option) : AlignInfo =
        match s, rng with
        | _, None -> AlignInfo(isValid=true)
        | s, Some rng when (rng.Start > rng.End) || (hasAnyInvalidRng isValidInAlignment s rng) ->
            AlignInfo(isValid=false)
        | s, Some rng ->
            let alignSubString = rng.GetSubString s
            let lastDashIdx = alignSubString.LastIndexOf('-')
            let width = match lastDashIdx with
                        | 0 -> int (alignSubString.Substring(1)) // skip dash for the numeric alignment
                        | -1 -> int alignSubString // no dash, must be all numeric
                        | _ -> 0 // dash is not allowed to be anywhere else
            if width = 0 then AlignInfo(isValid=false)
            else
                let isNegativeAlignWidth = lastDashIdx >= 0
                let direction = if isNegativeAlignWidth then Direction.Left else Direction.Right
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
    let inline emptyKeepTrying() = Unchecked.defaultof<TemplatePropertyValue>
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
        else emptyKeepTrying()

    let inline tryNullable (r:DestructureRequest) =
        let t = r.Value.GetType()
        let isNullable = t.IsConstructedGenericType && t.GetGenericTypeDefinition() = typedefof<Nullable<_>>
        if isNullable then
            match tryCastAs<System.Nullable<_>>(r.Value) with
            | n when (Object.ReferenceEquals(null, n)) -> (ScalarValue null)
            | n when (not n.HasValue) -> (ScalarValue null)
            | n when n.HasValue -> r.TryAgainWithValue(box (n.GetValueOrDefault()))
            | _ -> emptyKeepTrying()
        else emptyKeepTrying()

    let inline tryEnum (r:DestructureRequest) =
        match tryCastAs<System.Enum>(r.Value) with
        | e when (Object.ReferenceEquals(null, e)) -> emptyKeepTrying()
        | e -> (ScalarValue (e))

    let inline tryByteArrayMaxBytes (maxBytes:int) (r:DestructureRequest) =
        match tryCastAs<System.Byte[]>(r.Value) with
        | bytes when (Object.ReferenceEquals(null, bytes)) -> emptyKeepTrying()
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
        | _ -> emptyKeepTrying()

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
                        | ekt5 when isEmptyKeepTrying ekt5 -> emptyKeepTrying()
                        | tpv -> tpv
                    | tpv -> tpv
                | tpv -> tpv
            | tpv -> tpv
        | tpv -> tpv


    let inline tryNull (r:DestructureRequest) =
        match r.Value with | null -> ScalarValue null | _ -> emptyKeepTrying()
    let inline tryStringifyDestructurer (r:DestructureRequest) =
        match r.Hint with | DestrHint.Stringify -> ScalarValue (r.Value.ToString()) | _ -> emptyKeepTrying()

    let inline tryDelegateString (r:DestructureRequest) =
        if r.Hint <> DestrHint.Destructure then emptyKeepTrying()
        else
            match tryCastAs<System.Delegate>(r.Value) with
            | e when (Object.ReferenceEquals(null, e)) -> emptyKeepTrying()
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
        | e when Object.ReferenceEquals(null, e) -> emptyKeepTrying()
        | e when isScalarDict valueType ->
            let getKey o = o.GetType().GetRuntimeProperty("Key").GetValue(o)
            let getValue o = o.GetType().GetRuntimeProperty("Value").GetValue(o)
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
        if r.Hint <> DestrHint.Destructure then emptyKeepTrying()
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
                if i >= rzPubProps.Count then List.rev acc
                else
                    let pi = rzPubProps.[i]
                    try
                        let propValue = pi.GetValue(r.Value)
                        let propTpv = { Name=pi.Name; Value=r.TryAgainWithValue propValue }
                        loopDestrChildren (i+1) (propTpv :: acc)
                    with
                        | :? TargetParameterCountException as ex ->
                            r.Log("The property accessor {0} is a non-default indexer", [|pi|])
                            loopDestrChildren (i+1) (acc)
                        | :? TargetInvocationException as ex ->
                            r.Log("The property accessor {0} threw exception {1}", [| pi; ex; |])
                            let propValue = "The property accessor threw an exception:" + ex.InnerException.GetType().Name
                            let propTpv = { Name=pi.Name; Value=r.TryAgainWithValue propValue }
                            loopDestrChildren (i+1) (propTpv :: acc)

            let childStructureValues = loopDestrChildren 0 []
            StructureValue(typeTag, childStructureValues)

    /// A destructurer that does nothing by returning emptyKeepTrying()
    let inline alwaysKeepTrying (_:DestructureRequest) = emptyKeepTrying()

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
                                        | tpv when isEmptyKeepTrying tpv -> emptyKeepTrying()
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
    let inline isEmptyKeepTrying (tpv) = Destructure.isEmptyKeepTrying (tpv)

    let createCustomDestructurer (tryScalars:Destructurer option) (tryCustomObjects: Destructurer option) : Destructurer =
        Destructure.tryAllWithCustom tryScalars tryCustomObjects

    let defaultDestructureNoCustoms : Destructurer = Destructure.tryAllWithCustom None None

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
        else Destructure.emptyKeepTrying()

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
            | 0 -> fun _ -> Destructure.emptyKeepTrying()
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


/// Using functional ideoms from Chiron
module Next =
    // copy-n-paste from https://raw.githubusercontent.com/xyncro/aether/master/src/Aether/Aether.fs
    module Aether =

        (* Types

           Types defining lenses and isomorphisms (both total
           and partial as standard pairs. These can be implemented implicitly,
           so an assembly *providing* lenses without also consuming them
           requires no dependency on Aether, just an implicit structuring. *)

        /// Total lens from a -> b
        type Lens<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)

        /// Partial lens from a -> b
        type PLens<'a,'b> = ('a -> 'b option) * ('b -> 'a -> 'a)

        // Isomorphisms

        /// Total isomorphism of a <> b
        type Iso<'a,'b> = ('a -> 'b) * ('b -> 'a)

        /// Partial isomorphism of a <> b
        type PIso<'a,'b> = ('a -> 'b option) * ('b -> 'a)

        (* Functions

           Functions for using lenses to get, set and modify values within a target
           instance. *)

        [<RequireQualifiedAccess>]
        module Lens =

            /// Get a value using a total lens
            let get ((g, _): Lens<'a,'b>) = 
                fun a -> g a

            /// Get a value option using a partial lens
            let getPartial ((g, _): PLens<'a,'b>) = 
                fun a -> g a

            /// Get a value or a default using a partial lens
            let getPartialOrElse ((g, _): PLens<'a,'b>) = 
                fun b a -> g a |> function | Some b -> b | _ -> b

            /// Set a value using a total lens
            let set ((_, s): Lens<'a,'b>) =
                fun b a -> s b a

            /// Set a value using a partial lens
            let setPartial ((_, s): PLens<'a,'b>) =
                fun b a -> s b a

            /// Modify a value using a total lens
            let map ((g, s): Lens<'a,'b>) = 
                fun f a -> s (f (g a)) a

            /// Modify a value using a partial lens
            let mapPartial ((g, s): PLens<'a,'b>) = 
                fun f a -> Option.map f (g a) |> function | Some b -> s b a | _ -> a

        (* Compositions

           Functions for composing lenses and isomorphisms, each of which
           returns a new lens of a total or partial type based on the lenses
           or isomorphisms composed. It is more common (and significantly less
           verbose) to use the infix operator forms of these compositions (though note
           that Aether.Operators is not open by default and should be opened explicitly). *)

        [<RequireQualifiedAccess>]
        module Compose =

            /// Compose a total lens and a total lens, giving a total lens
            let totalLensTotalLens ((g1, s1): Lens<'a,'b>) ((g2, s2): Lens<'b,'c>) : Lens<'a,'c> =
                (fun a -> g2 (g1 a)),
                (fun c a -> s1 (s2 c (g1 a)) a)

            /// Compose a total lens and a partial lens, giving a partial lens
            let totalLensPartialLens ((g1, s1): Lens<'a,'b>) ((g2, s2): PLens<'b,'c>) : PLens<'a,'c> =
                (fun a -> g2 (g1 a)),
                (fun c a -> s1 (s2 c (g1 a)) a)

            /// Compose a partial lens and a total lens, giving a partial lens
            let partialLensTotalLens ((g1, s1): PLens<'a,'b>) ((g2, s2): Lens<'b,'c>) : PLens<'a,'c> =
                (fun a -> Option.map g2 (g1 a)),
                (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a | _ -> a)

            /// Compose two partial lenses, giving a partial lens
            let partialLensPartialLens ((g1, s1): PLens<'a,'b>) ((g2, s2): PLens<'b,'c>) : PLens<'a,'c> =
                (fun a -> Option.bind g2 (g1 a)),
                (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a | _ -> a)

            /// Compose a total lens with a total isomorphism, giving a total lens
            let totalLensTotalIsomorphism ((g, s): Lens<'a,'b>) ((f, t): Iso<'b,'c>) : Lens<'a,'c> =
                (fun a -> f (g a)),
                (fun c a -> s (t c) a)

            /// Compose a total lens with a partial isomorphism, giving a partial lens
            let totalLensPartialIsomorphism ((g, s): Lens<'a,'b>) ((f, t): PIso<'b,'c>) : PLens<'a,'c> =
                (fun a -> f (g a)),
                (fun c a -> s (t c) a)

            /// Compose a partial lens with a total isomorphism, giving a partial lens
            let partialLensTotalIsomorphism ((g, s): PLens<'a,'b>) ((f, t): Iso<'b, 'c>) : PLens<'a,'c> =
                (fun a -> Option.map f (g a)),
                (fun c a -> s (t c) a)

            /// Compose a partial lens with a partial isomorphism, giving a partial lens
            let partialLensPartialIsomorphism ((g, s): PLens<'a,'b>) ((f, t): PIso<'b,'c>) : PLens<'a,'c> =
                (fun a -> Option.bind f (g a)),
                (fun c a -> s (t c) a)

        (* Lenses

           Various lenses implemented for common types such as tuples,
           lists and maps, along with an id lens (which is useful for composing
           a lens which has not specific "lensing" elements but is implicitly a chain
           of one or more isomorphisms. Having an id lens enables the root composition. *)

        /// Identity lens returning the original item regardless of modifiction
        let idLens : Lens<'a,'a> =
            (fun x -> x), (fun x _ -> x) 

        /// First item of a tuple giving a total lens
        let fstLens : Lens<('a * 'b),'a> =
            fst, (fun a t -> a, snd t)
                
        /// Second item of a tuple giving a total lens
        let sndLens : Lens<('a * 'b),'b> =
            snd, (fun b t -> fst t, b)

        /// Head of a list giving a partial lens
        let headPLens : PLens<'v list, 'v> =
            (function | h :: _ -> Some h | _ -> None),
            (fun v -> function | _ :: t -> v :: t | l -> l)

        /// Position of a list giving a partial lens
        let listPLens (i: int) : PLens<'v list, 'v> =
            (function | l when List.length l > i -> Some (List.nth l i) | _ -> None), 
            (fun v l -> List.mapi (fun i' x -> if i = i' then v else x) l)

        /// Tail of a list giving a partial lens
        let tailPLens : PLens<'v list, 'v list> =
            (function | _ :: t -> Some t | _ -> None),
            (fun t -> function | h :: _ -> h :: t | [] -> t)

        /// Key of a map giving a partial lens
        let mapPLens (k: 'k) : PLens<Map<'k,'v>,'v> =
            Map.tryFind k, Map.add k

        (* Operators

           Operators are an optional feature of Aether and so must be explicitly opened
           when needed. *)

        module Operators =

            (* Composition Operators

               Operators as syntactical alternatives to more verbose composition
               functions given. These are expected to be much more commonly used
               and syntactially provide more clues as to their function. *)

            /// Compose a total lens and a total lens, giving a total lens
            let (>-->) l1 l2 =
                Compose.totalLensTotalLens l1 l2

            /// Compose a total lens and a partial lens, giving a partial lens
            let (>-?>) l1 l2 =
                Compose.totalLensPartialLens l1 l2

            /// Compose a partial lens and a total lens, giving a partial lens
            let (>?->) l1 l2 =
                Compose.partialLensTotalLens l1 l2

            /// Compose two partial lenses, giving a partial lens
            let (>??>) l1 l2 =
                Compose.partialLensPartialLens l1 l2

            /// Compose a total lens with a total isomorphism, giving a total lens
            let (<-->) l i =
                Compose.totalLensTotalIsomorphism l i

            /// Compose a total lens with a partial isomorphism, giving a partial lens
            let (<-?>) l i =
                Compose.totalLensPartialIsomorphism l i

            /// Compose a partial lens with a total isomorphism, giving a partial lens
            let (<?->) l i =
                Compose.partialLensTotalIsomorphism l i

            /// Compose a partial lens with a partial isomorphism, giving a partial lens
            let (<??>) l i =
                Compose.partialLensPartialIsomorphism l i

            (* Function Operators

               Operators as infix alternatives to some of the standard get, set,
               modify functions (getL, setL, etc.) Should likely be used rather 
               sparingly and in specific controlled areas unless you're aiming for 
               symbol soup. *)

            /// Get a value using a total lens
            let (^.) (a: 'a) (l: Lens<'a,'b>) : 'b =
                Lens.get l a

            /// Get a value using a partial lens
            let (^?.) (a: 'a) (l: PLens<'a,'b>) : 'b option =
                Lens.getPartial l a

            /// Set a value using a total lens
            let (^=) (b: 'b) (l: Lens<'a,'b>) : 'a -> 'a =
                Lens.set l b

            /// Set a value using a partial lens
            let (^?=) (b: 'b) (l: PLens<'a,'b>) : 'a -> 'a =
                Lens.setPartial l b

            /// Modify a value using a total lens
            let (^%=) (f: 'b -> 'b) (l: Lens<'a,'b>) : 'a -> 'a =
                Lens.map l f

            /// Modify a value using a partial lens
            let (^?%=) (f: 'b -> 'b) (l: PLens<'a,'b>) : 'a -> 'a =
                Lens.mapPartial l f

    open System
    open Aether
    open Aether.Operators

    type ContentType = string

    type Value =
        | String of string
        | Bool of bool // NOTE: new
        | Float of decimal // NOTE: changed from float
        | Int64 of int64
        | BigInt of bigint
        | Binary of byte [] * ContentType
        | Fraction of int * int
        | Object of Map<string, Value> // NOTE: moved from ComplexValue
        | Array of Value list // NOTE: moved from ComplexValue

        (* Isomorphisms *)

        static member StringPIso : PIso<Value, string> =
            (function | String x -> Some x
                      | _ -> None), String

        static member BoolPIso : PIso<Value, bool> =
            (function | Bool x -> Some x
                      | _ -> None), Bool

        static member FloatPIso : PIso<Value, decimal> =
            (function | Float x -> Some x
                      | _ -> None), Float

        static member IntPIso : PIso<Value, int64> =
            (function | Int64 x -> Some x
                      | _ -> None), Int64

        static member BigIntPIso : PIso<Value, bigint> =
            (function | BigInt x -> Some x
                      | _ -> None), BigInt

        static member BinaryPIso : PIso<Value, byte [] * ContentType> =
            (function | Binary (bs, ct) -> Some (bs, ct)
                      | _ -> None), Binary

        static member FractionPIso : PIso<Value, int * int> =
            (function | Fraction (n, d) -> Some (n, d)
                      | _ -> None), Fraction

        static member ArrayPIso : PIso<Value, Value list> =
            (function | Array x -> Some x
                      | _ -> None), Array

        static member ObjectPIso : PIso<Value, Map<string, Value>> =
            (function | Object x -> Some x
                      | _ -> None), Object

        (* Lenses *)

        static member StringPLens : PLens<Value, string> =
            idLens <-?> Value.StringPIso

        static member BoolPLens : PLens<Value, bool> =
            idLens <-?> Value.BoolPIso

        static member FloatPLens : PLens<Value, decimal> =
            idLens <-?> Value.FloatPIso

        static member IntPLens : PLens<Value, int64> =
            idLens <-?> Value.IntPIso

        static member BigIntPLens : PLens<Value, bigint> =
            idLens <-?> Value.BigIntPIso

        static member BinaryPLens : PLens<Value, byte[] * ContentType> =
            idLens <-?> Value.BinaryPIso

        static member FractionPLens : PLens<Value, int * int> =
            idLens <-?> Value.FractionPIso

        static member ArrayPLens : PLens<Value, Value list> =
            idLens <-?> Value.ArrayPIso

        static member ObjectPLens : PLens<Value, Map<string, Value>> =
            idLens <-?> Value.ObjectPIso

    module Escaping =
        let private unescaped i =
                i >= 0x20 && i <= 0x21
             || i >= 0x23 && i <= 0x5b
             || i >= 0x5d && i <= 0x10ffff

        let escape (s: string) =
            let rec escape r =
                function | [] -> r
                         | h :: t when (unescaped (int h)) ->
                            escape (r @ [ h ]) t
                         | h :: t ->
                            let n =
                                match h with
                                | '"' -> [ '\\'; '"' ]
                                | '\\' -> [ '\\'; '\\' ]
                                | '\b' -> [ '\\'; 'b' ]
                                | '\f' -> [ '\\'; 'f' ]
                                | '\n' -> [ '\\'; 'n' ]
                                | '\r' -> [ '\\'; 'r' ]
                                | '\t' -> [ '\\'; 't' ]
                                | x -> [ '\\'; 'u' ] @ [ for c in ((int x).ToString ("X4")) -> unbox c ]

                            escape (r @ n) t

            new string (List.toArray (escape [] [ for c in s -> unbox c ]))

    module Destructure =
        ()

    [<AutoOpen>]
    module Capture =

        // TODO: this structure is both a Reader and Writer monad, but only needs
        // to be a writer monad
        type Value<'a> =
            Value -> ValueResult<'a> * Value

        and ValueResult<'a> =
            | ValueResult of 'a
            | Error of string

        [<RequireQualifiedAccess>]
        module Value =

            let inline init (a: 'a) : Value<'a> = 
                fun json ->
                    ValueResult a, json

            let inline error (e: string) : Value<'a> =
                fun json ->
                    Error e, json

            let inline internal ofResult result =
                fun json ->
                    result, json

            let inline bind (m: Value<'a>) (f: 'a -> Value<'b>) : Value<'b> =
                fun json ->
                    match m json with
                    | ValueResult a, json -> (f a) json
                    | Error e, json -> Error e, json

            let inline apply (f: Value<'a -> 'b>) (m: Value<'a>) : Value<'b> =
                bind f (fun f' ->
                    bind m (fun m' ->
                        init (f' m')))

            let inline map (f: 'a -> 'b) (m: Value<'a>) : Value<'b> =
                bind m (fun m' ->
                    init (f m'))

            let inline map2 (f: 'a -> 'b -> 'c) (m1: Value<'a>) (m2: Value<'b>) : Value<'c> =
                apply (apply (init f) m1) m2

    (* Operators

       Symbolic operators for working with Value<'a> functions, providing
       an operator based concise alternative to the primitive Json<'a> combinators
       given as part of Functional.
       
       This module is not opened by default, as symbolic operators are a matter
       of taste and may also clash with other operators from other libraries. *)

    module Operators =

        let inline (>>=) m f =
            Value.bind m f

        let inline (=<<) f m =
            Value.bind m f

        let inline (<*>) f m =
            Value.apply f m

        let inline (<!>) f m =
            Value.map f m

        let inline (>>.) m f =
            Value.bind m (fun _ -> f)

        let inline (.>>) m f =
            Value.bind (fun _ -> m) f

        let inline ( *>) m1 m2 =
            Value.map2 (fun _ x -> x) m1 m2

        let inline ( <*) m1 m2 =
            Value.map2 (fun x _ -> x) m1 m2

        let inline (>=>) m1 m2 =
            Value.bind (fun x -> m1 x) m2

        let inline (<=<) m1 m2 =
            Value.bind (fun x -> m2 x) m1

    [<AutoOpen>]
    module Formatting =

        (* Helpers *)

        type private Formatter<'a> =
            'a -> StringBuilder -> StringBuilder

        type private Separator =
            StringBuilder -> StringBuilder

        let private append (s: string) (b: StringBuilder) =
            b.Append s

        let private appendf (s: string) (v1: obj) (b: StringBuilder) =
            b.AppendFormat (s, v1)

        let private join<'a> (f: Formatter<'a>) (s: Separator) =
            let rec join values (b: StringBuilder) =
                match values with
                | [] -> b
                | h :: [] -> f h b
                | h :: t -> (f h >> s >> join t) b

            join

        (* Formatters *)

        let rec private formatValue =
                function | String x -> formatString x
                         | Bool x -> formatBool x
                         | Float x -> formatFloat x
                         | Int64 x -> formatInt x
                         | BigInt x -> formatBigInt x
                         | Binary (bs, ct) -> formatBinary (bs, ct)
                         | Fraction (n, d) -> formatFraction (n, d)
                         | Object x -> formatObject x
                         | Array x -> formatArray x

        and private formatArray =
            function | x ->
                           append "[" 
                        >> join formatValue (append ",") x 
                        >> append "]"

        and private formatFloat =
            function | x -> append (string x)

        and private formatBool =
            function | x -> append (string x)

        and private formatInt =
            function | x -> append (string x)

        and private formatBigInt =
            function | x -> append (string x)

        and private formatBinary =
            function | (bs, ct) -> append (BitConverter.ToString(bs).Replace("-", ""))

        and private formatFraction =
            function | (n, d) -> append ("[ " + string n + " / " + string d + " ]")

        and private formatObject =
            function | x -> 
                           append "{" 
                        >> join (fun (k, v) -> appendf "\"{0}\":" (Escaping.escape k) >> formatValue v)
                                (append ",")
                                (Map.toList x) 
                        >> append "}"

        and private formatString =
            function | x -> appendf "\"{0}\"" (Escaping.escape x)

        [<RequireQualifiedAccess>]
        module Value =

            let format value =
                StringBuilder ()
                |> formatValue value
                |> string

    (* Lens

       Functional lens based access to nested Json data strcutures,
       using Aether format lenses. Uses Value<'a> based functions, so
       can be used monadicly. *)

    [<AutoOpen>]
    module Lens =

        (* Functions *)

        [<RequireQualifiedAccess>]
        module Value =

            let getLens l : Value<_> =
                fun value ->
                    ValueResult (Lens.get l value), value

            let getLensPartial l : Value<_> =
                fun value ->
                    match Lens.getPartial l value with
                    | Some x -> ValueResult x, value
                    | _ -> Error (sprintf "couldn't use lens %A on value '%A'" l value), value

            let tryGetLensPartial l : Value<_> =
                fun value ->
                    ValueResult (Lens.getPartial l value), value

            let setLens l v : Value<_> =
                fun value ->
                    ValueResult (), Lens.set l v value

            let setLensPartial l v : Value<_> =
                fun value ->
                    ValueResult (), Lens.setPartial l v value

            let mapLens l f : Value<_> =
                fun value ->
                    ValueResult (), Lens.map l f value

            let mapLensPartial l f : Value<_> =
                fun value ->
                    ValueResult (), Lens.mapPartial l f value

    [<AutoOpen>]
    module Mapping =

        open Operators

        (* To
        
            *)

        (* Defaults *)

        type ToValueDefaults = ToValueDefaults with

            (* Basic Types *)

            static member inline ToValue (x: bool) =
                Value.setLensPartial Value.BoolPLens x

            static member inline ToValue (x: decimal) =
                Value.setLensPartial Value.FloatPLens x

            static member inline ToValue (x: float) =
                Value.setLensPartial Value.FloatPLens (decimal x)

            static member inline ToValue (x: int) =
                Value.setLensPartial Value.FloatPLens (decimal x)

            static member inline ToValue (x: int16) =
                Value.setLensPartial Value.IntPLens (int64 x)

            static member inline ToValue (x: int64) =
                Value.setLensPartial Value.IntPLens x

            static member inline ToValue (x: single) =
                Value.setLensPartial Value.FloatPLens (decimal x)

            static member inline ToValue (x: string) =
                Value.setLensPartial Value.StringPLens x

            static member inline ToValue (x: uint16) =
                Value.setLensPartial Value.IntPLens (int64 x)

            static member inline ToValue (x: uint32) =
                Value.setLensPartial Value.IntPLens (int64 x)

            static member inline ToValue (x: uint64) =
                Value.setLensPartial Value.FloatPLens (decimal x)

            (* Common Types *)

            static member inline ToValue (x: DateTime) =
                Value.setLensPartial Value.StringPLens (x.ToUniversalTime().ToString("o"))
            
            static member inline ToValue (x: DateTimeOffset) =
                Value.setLensPartial Value.StringPLens (x.ToString("o"))

            static member inline ToValue (x: Guid) =
                Value.setLensPartial Value.StringPLens (string x)

            (* Json Type *)

            static member inline ToValue (x: Value) =
                Value.setLens idLens x

        (* Mapping Functions

           Functions for applying the ToJson function to data structures to produce
           new Json instances. *)

        let inline internal toValueDefaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member ToValue: ^a -> unit Value) a)

        let inline internal toValue (x: 'a) =
            snd (toValueDefaults (x, ToValueDefaults) (Object (Map.empty)))

        (* Defaults *)

        type ToValueDefaults with

            (* Arrays *)

            static member inline ToValue (x: 'a array) =
                Value.setLens idLens (Array ((Array.toList >> List.map toValue) x))

            (* Lists *)

            static member inline ToValue (x: 'a list) =
                Value.setLens idLens (Array (List.map toValue x))

            (* Maps *)

            static member inline ToValue (x: Map<string,'a>) =
                Value.setLens idLens (Object (Map.map (fun _ a -> toValue a) x))

            (* Options *)

            static member inline ToValue (x: 'a option) =
                match x with | None -> Value.init ()
                             | Some a -> Value.setLens idLens (toValue a)

            (* Sets *)

            static member inline ToValue (x: Set<'a>) =
                Value.setLens idLens (Array ((Set.toList >> List.map toValue) x))

            (* Tuples *)

            static member inline ToValue ((a, b)) =
                Value.setLens idLens (Array [ toValue a; toValue b ])

            static member inline ToValue ((a, b, c)) =
                Value.setLens idLens (Array [ toValue a; toValue b; toValue c ])

        [<RequireQualifiedAccess>]
        module Value =

            let inline write key value =
                Value.setLensPartial (Value.ObjectPLens >??> mapPLens key) (toValue value)

            let inline serialize a =
                toValue a

    type BaseUnit =
        | Bit
        | Byte
        | Second
        | Metre
        | Scalar
        | Ampere
        | Kelvin
        | Mole
        | Candela

    type Units =
        | BaseUnit of BaseUnit
        | Mul of BaseUnit * BaseUnit
        | Pow of BaseUnit * BaseUnit
        | Div of BaseUnit * BaseUnit
        | Root of BaseUnit
        | Sqrt of BaseUnit
        | Log10 of BaseUnit // Log of base:float * BaseUnit

    type LogContext =
        { datacenter : string
          hostname   : string
          service    : string
          ns         : string option
          func       : string option
          file       : string option
          lineNo     : uint32 option
          envVersion : string }

        static member Create(service : string) =
          { datacenter = "dc1"
            hostname   = "coinduction"
            service    = service
            ns         = None
            func       = None
            file       = None
            lineNo     = None
            envVersion = "0.0.0" }

    type PointName = string list

    type PointValue =
        /// Value at point in time
        | Gauge of Value * Units
        /// Any sort of derived measure
        | Derived of Value * Units
        /// All simple-valued fields' values can be templated into the template string
        /// when outputting the value in the target.
        | Event of template:string

    type Field = Field of Value * Units option // move outside this module
        
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] // remove when field moved outside
    module Field =

        let inline init key value units =
            
            Field ((Value.serialize value), units)

    type Message =
      { name      : PointName
        value     : PointValue
        /// the semantic-logging data
        fields    : Map<PointName, Field>
        /// the principal/actor/user/tenant/act-as/oauth-id data
        session   : Value // NOTE: changed from Map<PointName, Field>
        /// where in the code?
        context   : LogContext
        /// what urgency?
        level     : string
        /// when?
        timestamp : int64 }

        static member fields_ : Lens<Message, Map<PointName, Field>> =
            (fun x -> x.fields),
            (fun v x -> { x with fields = v })

    type Logger =
        abstract log : Message -> unit

    type Logging =
        static member get(modulee, file, line) =
            { new Logger with
                member x.log m = () }
        
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Message =

        let format (template:string) (args : Value seq) =
            let template = Parser.parse template
            let sb       = StringBuilder()
            captureThenFormat sb template args

        let formatf (pf:PrintfFormat<_,_,_,_,'t>) : _ -> 't =
            format pf.Value



module Usage =
    open Next
    open Next.Aether
    open Next.Operators

    let (<+>) xs x = 
        xs @ x

    let logger =
        Logging.get(modulee="MessageTemplates.Usage", file=__SOURCE_FILE__, line=__LINE__)

    let sample () =
        let values =
                Value.write "bytes" 4
            <+> Value.write "sessionId" "session-haf25643256"

        let msg =
            Message.format "read ${bytes} on connection ${connectionId}"
            |> Lens.set Message.fields_ (Field.init "bytes" 4 (BaseUnit Byte))

        let msg' =
            Message.format "read ${bytes} on connection ${connectionId}"
            |> Field.initWithUnit "bytes" 4 Byte
            |> Field.init "connectionId" "session-haf23456743"

        msg
