module FsMessageTemplates.MessageTemplates

type DestructureKind = Default = 0 | Stringify = 1 | Destructure = 2

type TokenData = { StartIndex:int
                   Text: string }
    with static member Empty = { StartIndex = 0; Text = "" }
         override x.ToString() = x.Text

type Direction = Left = 0 | Right = 1

type AlignInfo = { Direction: Direction
                   Width: int }
    with static member Default = { Direction=Direction.Left; Width=(0) }
         override x.ToString() =
            let dir = if x.Direction = Direction.Right then "-" else ""
            dir + string x.Width

type PropertyData = { Name: string
                      Pos: int option
                      Destr: DestructureKind
                      Align: AlignInfo option
                      Format: string option }
    with
        member x.IsPositional with get() = x.Pos.IsSome
        static member Empty = { Name=""; Pos=None; Destr=DestructureKind.Default; Align=None; Format=None }
        override x.ToString() = sprintf "{%s%s%s%s}"
                                        (match x.Destr with
                                         | DestructureKind.Destructure -> "@"
                                         | DestructureKind.Stringify -> "$"
                                         | DestructureKind.Default
                                         | _ -> "")
                                        x.Name
                                        (match x.Align with | Some a -> "," + string a | _ -> "")
                                        (match x.Format with | Some f -> ":" + f | _ -> "")

type Token =
| Text of TokenData
| Prop of TokenData * PropertyData
    with static member EmptyText = Text(TokenData.Empty)
         static member EmptyProp = Prop(TokenData.Empty, PropertyData.Empty)
         override x.ToString() = match x with
                                 | Text t -> t.Text
                                 | Prop (t, p) -> sprintf "%s->%s" (string t) (string p)

type Template = { FormatString: string; Tokens: Token list }

module Tk =
    let td tindex raw = { Text=raw; StartIndex=tindex }
    let text tindex raw = Token.Text(td tindex raw)
    let emptyProp = PropertyData.Empty
    let prop tindex raw name = Token.Prop(td tindex raw, { emptyProp with Name=name })
    let propf tindex raw name format = Token.Prop(td tindex raw, { emptyProp with Name=name; Format=Some format })
    let propd tindex raw name = Token.Prop(td tindex raw, { emptyProp with Name=name; Destr = DestructureKind.Destructure })
    let propds tindex raw name = Token.Prop(td tindex raw, { emptyProp with Name=name; Destr = DestructureKind.Stringify })
    let propar tindex raw name rightWidth = Token.Prop(td tindex raw, { emptyProp with Name=name; Align=Some { Direction=Direction.Right; Width=rightWidth } })
    let propal tindex raw name leftWidth = Token.Prop(td tindex raw, { emptyProp with Name=name; Align=Some { Direction=Direction.Left; Width=leftWidth } })
    let propp tindex num = Token.Prop({ Text="{"+string num+"}"; StartIndex=tindex; }, { emptyProp with Name=string num; Pos=Some num })

let tryParseInt32 s = System.Int32.TryParse(s, System.Globalization.NumberStyles.None, System.Globalization.CultureInfo.InvariantCulture)
let maybeInt32 s =
    if System.String.IsNullOrEmpty(s) then None
    else match tryParseInt32 s with true, n -> Some n | false, _ -> None

let parseTextToken (startAt:int) (template:string) (callersNextIndex: int ref) : Token =
    let first = startAt
    let mutable next = startAt
    let inline moveToNextChar() = next <- next + 1
    let accum = System.Text.StringBuilder()
    let mutable isDone = false
    let inline addCharToThisTextToken (c:char) = accum.Append(c) |> ignore
    let inline isNextChar (c:char) = next+1 < template.Length && template.[next+1] = c

    while not isDone do
        let thisChar = template.[next]
        if thisChar = '{' then
            if isNextChar '{' then addCharToThisTextToken thisChar; moveToNextChar()
            else
                isDone <- true // maybe a property, stop here
        else
            addCharToThisTextToken thisChar
            if thisChar = '}' && isNextChar '}' then moveToNextChar () // treat "}}" as "}"

        if not isDone || thisChar <> '{' then moveToNextChar()
        isDone <- isDone || next >= template.Length

    callersNextIndex := next
    if accum.Length > 0 then Tk.text first (accum.ToString())
    else Token.EmptyText

let parsePropertyToken (startAt:int) (messageTemplate:string) (callersNextIndex: int ref) : Token =
    let first = startAt
    let mutable thisChIdx = startAt
    let inline peekChAt (index) = messageTemplate.[index]
    let inline isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    let inline isDigit c = (c >= '0' && c <= '9')
    let inline isLetterOrDigit c = isDigit c || isLetter c
    let inline isValidInPropName c = c = '_' || isLetterOrDigit c
    let inline isValidInDestrHint c = c = '@' || c = '$'
    let inline isValidInAlignment c = c = '-' || isDigit c
    let inline isValidInFormat c = c <> '}' && (c = ' ' || isLetterOrDigit c || System.Char.IsPunctuation c)
    let inline isValidCharInPropTag c = c = ':' || isValidInPropName c || isValidInFormat c || isValidInDestrHint c
    let inline isAnyInvalid isValid (s:string) =
        let len = s.Length
        let rec go i =
            if i >= len then false
            else if (isValid s.[i]) = true then go (i+1) else true
        go 0

    /// "pname"                 > (valid=true, pnds="pname", format=None,             align=None        )
    /// "@abc:000"              > (valid=true, pnds="@abc",  format=Some "000",       align=None        )
    /// "$foo,-10:dd mmm yy"    > (valid=true, pnds="$foo",  format=Some "dd mmm yy", align=Some "-10"  )
    let inline trySplitTagContent (tagContent: string) : bool * string * string option * string option =
        match tagContent.IndexOf(':'), tagContent.IndexOf(',') with
        | -1, -1 -> // neither align nor format 
            true, tagContent, None, None
        | fmtIdx, -1 -> // has format part, but does not have align part
            true, tagContent.Substring(0, fmtIdx), Some (tagContent.Substring (fmtIdx+1)), None
        | -1, alIdx -> // has align part, but does not have format part
            true, tagContent.Substring(0, alIdx), None, Some (tagContent.Substring (alIdx+1))
        | fmtIdx, alIdx when alIdx < fmtIdx && alIdx <> (fmtIdx - 1) -> // has both parts in correct order
            let align = Some (tagContent.Substring (alIdx+1, fmtIdx - alIdx - 1))
            let fmt = Some (tagContent.Substring (fmtIdx + 1))
            true, tagContent.Substring(0, alIdx), fmt, align
        | fmtIdx, alIdx when alIdx > fmtIdx -> // has format part, no align (but comma in format string)
            let fmt = Some (tagContent.Substring (fmtIdx + 1))
            true, tagContent.Substring(0, fmtIdx), fmt, None
        | _, _ -> false, "", None, None // hammer time; you can't split this
    
    let inline tryParseAlignInfo (s:string option) : bool * AlignInfo option =
        match s with
        | None -> true, None
        | Some s when (isAnyInvalid isValidInAlignment s) -> false, None
        | Some "" -> false, None
        | Some s ->
            let lastDashIdx = s.LastIndexOf('-')
            let width = match lastDashIdx with
                        | 0 -> int (s.Substring(1)) // skip dash for the numeric alignment
                        | -1 -> int s // no dash, must be all numeric
                        | _ -> 0 // dash is not allowed to be anywhere else
            if width = 0 then false, None
            else
                let direction = match lastDashIdx with -1 -> Direction.Left | _ -> Direction.Right
                true, Some { Direction = direction; Width=width; }

    // skip over characters until we reach a character that is *NOT* a valid part of
    // the property tag
    while thisChIdx < messageTemplate.Length && isValidCharInPropTag(peekChAt thisChIdx) do
        thisChIdx <- thisChIdx + 1

    // if we stopped at the end of the string or the last char wasn't a close brace
    // then we treat all the characters we found as a text token, and finish.
    if thisChIdx = messageTemplate.Length || peekChAt thisChIdx <> '}' then
        callersNextIndex := thisChIdx
        Tk.text first (messageTemplate.Substring(first, thisChIdx - first))
    else
        let nextIndex = thisChIdx + 1
        callersNextIndex := nextIndex

        let rawText = messageTemplate.Substring(first, nextIndex - first)
        let tagContent = messageTemplate.Substring(first + 1, nextIndex - (first + 2))
        match trySplitTagContent tagContent with
        | true, nameAndDestr, format, align ->
            let destr = match nameAndDestr.[0] with
                        | '@' -> DestructureKind.Destructure
                        | '$' -> DestructureKind.Stringify
                        | _ -> DestructureKind.Default
            let propertyName = match destr with
                               | DestructureKind.Default -> nameAndDestr
                               | _ -> nameAndDestr.Substring(1)
            if propertyName = "" || (isAnyInvalid isValidInPropName propertyName) then
                Tk.text first rawText // not a valid property
            else
                if format.IsSome && (isAnyInvalid isValidInFormat format.Value) then
                    Tk.text first rawText 
                else
                    match tryParseAlignInfo align with
                    | false, _ -> Tk.text first rawText
                    | true, alignInfo ->
                        Prop({ StartIndex=first; Text=rawText },
                             { Name=propertyName; Format=format; Align=alignInfo; Pos=maybeInt32 propertyName; Destr=destr })

        | false, _, _, _ -> Tk.text first rawText

let parseTokens messageTemplate = seq {
    if messageTemplate = "" then yield Token.EmptyText
    else
        let isDone = ref false
        let nextIndex = ref 0
        while not !isDone do
            let beforeText = !nextIndex
            let tt = parseTextToken !nextIndex messageTemplate nextIndex
            if !nextIndex > beforeText then
                yield tt
            if !nextIndex = messageTemplate.Length then
                isDone := true
            if not !isDone then
                let beforeProp = !nextIndex
                let pt = parsePropertyToken !nextIndex messageTemplate nextIndex
                if !nextIndex > beforeProp then
                    yield pt // a property was parsed, since the nextIndex was moved forward
                if !nextIndex = messageTemplate.Length then
                    isDone := true
}

let parse (s:string) =
    { FormatString = s; Tokens = s |> parseTokens |> Seq.toList }

let captureProperties (t:Template) (args:obj[]) : (PropertyData * obj) seq = Seq.empty // todo

/// Converts a template message in a System.String.Format (positional) template
/// e.g. "abc {@def}" would become "abc {0}"
let createPositionalFormat (t: Template) =
    let tokenToPositionalFormat getNextPropPos token =
        let formatOrEmpty = function Some "" | None -> "" | Some s -> ":" + s
        let directionOrEmpty (width:int) =
            function | Direction.Right -> ",-" + string width | Direction.Left | _ -> "," + string width
        let alignToFormat = function Some a -> directionOrEmpty a.Width a.Direction | None -> ""
        match token with
        | Text ti -> ti.Text
        | Prop (_, pi) ->
            let sb = System.Text.StringBuilder()
            let inline append (s:string) = sb.Append s |> ignore
            append "{"
            append (string (getNextPropPos()))
            if pi.Align.IsSome then
                append (alignToFormat pi.Align)
            if pi.Format.IsSome then
                append (formatOrEmpty pi.Format)
            append ("}")
            sb.ToString()

    // generate numbers starting at zero, increasing +1 each call
    let nextPos =
        let i = ref -1
        fun () -> incr i; !i

    let strings = t.Tokens |> Seq.map (tokenToPositionalFormat nextPos) |> Seq.toArray
    System.String.Join("", strings)

let format provider template values =
    System.String.Format(provider, createPositionalFormat template, values)


