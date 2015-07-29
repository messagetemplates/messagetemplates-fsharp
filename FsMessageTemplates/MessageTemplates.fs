module FsMessageTemplates.MessageTemplates

type DestructureKind = Default = 0 | Stringify = 1 | Destructure = 2
let getDestrFromChar = function
                       | '@' -> DestructureKind.Destructure
                       | '$' -> DestructureKind.Stringify
                       | _ -> DestructureKind.Default
        
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
type PropertyData(name:string, pos:int option, destr:DestructureKind, align: AlignInfo option, format: string option) =
    member __.Name = name
    member __.Pos = pos
    member __.Destr = destr
    member __.Align = align
    member __.Format = format
    with
        member x.IsPositional with get() = x.Pos.IsSome
        static member Empty = PropertyData("", None, DestructureKind.Default, None, None)
        override x.ToString() =
            let sb = System.Text.StringBuilder()
            let append (s:string) = sb.Append(s) |> ignore
            append "{"
            if x.Destr <> DestructureKind.Default then
                append (match x.Destr with DestructureKind.Destructure -> "@" | DestructureKind.Stringify -> "$" | _ -> "")
            append (x.Name)
            if x.Align <> None then append ","; append (string x.Align.Value)
            if x.Format <> None then append ":"; append (string x.Format.Value)
            sb.ToString()

type Token =
| Text of startIndex:int * text:string
| Prop of startIndex:int * PropertyData
    with static member EmptyText = Text(emptyTokenData)
         static member EmptyProp = Prop(0, PropertyData.Empty)
         override x.ToString() = match x with
                                 | Text (_, s) -> s
                                 | Prop (_, pd) -> (string pd)

type Template = { FormatString: string; Tokens: Token list }

module Tk =
    let text tindex raw = Token.Text(tindex, raw)

let tryParseInt32 s = System.Int32.TryParse(s, System.Globalization.NumberStyles.None, System.Globalization.CultureInfo.InvariantCulture)
let maybeInt32 s =
    if System.String.IsNullOrEmpty(s) then None
    else match tryParseInt32 s with true, n -> Some n | false, _ -> None

let parseTextToken (startAt:int) (template:string) : int*Token =
    let tlen = template.Length
    let sb = System.Text.StringBuilder()
    let inline append (ch:char) = sb.Append(ch) |> ignore
    let rec go i =
        if i >= tlen then tlen, Text(startAt, sb.ToString())
        else
            let c = template.[i]
            match c with
            | '{' ->
                if (i+1) < tlen && template.[i+1] = '{' then append c; go (i+2) (*c::acc*) // treat "{{" and a single "{"
                else
                    // start of a property, bail out here
                    if i = startAt then startAt, Token.EmptyText
                    else i, Text(startAt, sb.ToString())
            | '}' when (i+1) < tlen && template.[i+1] = '}' -> // treat "}}" as a single "}"
                append c
                go (i+2) (*c::acc*) 
            | _ ->
                // append this char and keep going
                append c
                go (i+1) (*c::acc*)
    go startAt

let inline isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let inline isDigit c = (c >= '0' && c <= '9')
let inline isLetterOrDigit c = isDigit c || isLetter c
let inline isValidInPropName c = c = '_' || isLetterOrDigit c
let inline isValidInDestrHint c = c = '@' || c = '$'
let inline isValidInAlignment c = c = '-' || isDigit c
let inline isValidInFormat c = c <> '}' && (c = ' ' || isLetterOrDigit c || System.Char.IsPunctuation c)
let inline isValidCharInPropTag c = c = ':' || isValidInPropName c || isValidInFormat c || isValidInDestrHint c
let inline tryGetFirstChar predicate (s:string) first =
    let len = s.Length
    let rec go i =
        if i >= len then None
        else if not (predicate s.[i]) then go (i+1) else Some i
    go first

[<Struct>]
type Range(startPos:int, endPos:int) =
    member __.Start = startPos
    member __.End = endPos
    member __.GetSubString (s:string) = s.Substring(startPos, (endPos - startPos) + 1)
    override __.ToString() = (string startPos) + ", " + (string endPos)

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
                           PropertyData(propertyName, maybeInt32 propertyName, destr, alignInfo, format)))

let parsePropertyToken (startAt:int) (messageTemplate:string) : int*Token =
    let tlen = messageTemplate.Length
    let first = startAt
    
    // skip over characters until we reach a character that is *NOT* a valid part of
    // the property tag
    let nextInvalidIdx = match tryGetFirstChar (not << isValidCharInPropTag) messageTemplate (first+1) with
                         | Some idx -> idx
                         | None _ -> tlen

    // if we stopped at the end of the string or the last char wasn't a close brace
    // then we treat all the characters we found as a text token, and finish.
    if nextInvalidIdx = tlen || messageTemplate.[nextInvalidIdx] <> '}' then
        nextInvalidIdx, Tk.text first (messageTemplate.Substring(first, nextInvalidIdx - first))
    else
        // skip over the trailing "}" close prop tag
        let nextIndex = nextInvalidIdx + 1
        let propInsidesRng = Range(first + 1, nextIndex - 2)
        match tryGetPropInSubString messageTemplate propInsidesRng with
        | Some p -> nextIndex, p
        | None _ -> nextIndex, Tk.text first (messageTemplate.Substring(first, nextIndex - first))

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

let captureProperties (t:Template) (args:obj[]) : (PropertyData * obj) seq = Seq.empty // todo

/// Converts a template message in a System.String.Format (positional) template
/// e.g. "abc {@def}" would become "abc {0}"
let createPositionalFormat (t: Template) =
    let sb = System.Text.StringBuilder()
    let inline append (s:string) = sb.Append s |> ignore
    let formatOrEmpty = function Some "" | None -> "" | Some s -> ":" + s
    let directionOrEmpty (width:int) =
        function | Direction.Right -> ",-" + string width | Direction.Left | _ -> "," + string width
    let alignToFormat = function Some (a:AlignInfo) -> directionOrEmpty a.Width a.Direction | None -> ""

    let rec appendAllPositional (items:Token list) nextPosNum =
        match items.Head with
        | Text (_, s) ->
            append s
            if items.Tail.IsEmpty then ()
            else appendAllPositional items.Tail nextPosNum
        | Prop (_, pd) ->
            append "{"
            append (string nextPosNum)
            if pd.Align.IsSome then append (alignToFormat pd.Align)
            if pd.Format.IsSome then append (formatOrEmpty pd.Format)
            append ("}")
            if items.Tail.IsEmpty then ()
            else appendAllPositional items.Tail (nextPosNum+1)

    appendAllPositional t.Tokens 0
    sb.ToString()

let format provider template values =
    System.String.Format(provider, createPositionalFormat template, values)


