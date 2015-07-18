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
                                        // (match x.Pos with | Some p -> string p | _ -> "") // already part of name ?
                                        (match x.Align with | Some a -> string a | _ -> "")
                                        (match x.Format with | Some f -> f | _ -> "")

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
    let text tindex raw = Token.Text({ Text = raw; StartIndex = tindex })        

    let prop tindex raw name = Token.Prop({ Text=raw; StartIndex=tindex },
                                         { PropertyData.Empty with Name=name })
    let propf tindex raw name format =
        Token.Prop({ Text=raw; StartIndex=tindex },
                   { PropertyData.Empty with Name=name
                                             Format=Some format })

    let propd tindex raw name = Token.Prop({ Text=raw; StartIndex=tindex; },
                                          { PropertyData.Empty with Name=name
                                                                    Destr = DestructureKind.Destructure })
    let propds tindex raw name = Token.Prop({ Text=raw; StartIndex=tindex; },
                                           { PropertyData.Empty with Name=name
                                                                     Destr = DestructureKind.Stringify })
    let propar tindex raw name rightWidth =
        Token.Prop({ Text=raw; StartIndex=tindex; },
                   { PropertyData.Empty with Name=name
                                             Align=Some { Direction=Direction.Right
                                                          Width=rightWidth } })

    let propal tindex raw name leftWidth =
        Token.Prop({ Text=raw; StartIndex=tindex; },
                   { PropertyData.Empty with Name=name
                                             Align=Some { Direction=Direction.Left
                                                          Width=leftWidth } })
    let propp tindex num =
        let snum = string num
        Token.Prop({ Text="{"+snum+"}"; StartIndex=tindex; },
               { PropertyData.Empty with Name=snum; Pos=Some num })

open System.Text.RegularExpressions

[<Literal>]
let regexOptions =
#if MT_PORTABLE
    RegexOptions.Compiled |||
#endif
    RegexOptions.IgnorePatternWhitespace

module GRP =
    let DOUBLE_OPEN = "double_open"
    let DOUBLE_CLOSE = "double_close"
    let FULL_PROP = "fullprop"
    let START_PROP = "start_prop"
    let DESTR = "destr"
    let PROPERTY = "property"
    let ALIGN = "align"
    let FORMAT = "format"
    let END_PROP = "end_prop"
    let OTHER_TEXT = "other_text"

// https://www.debuggex.com/r/4t9Y20IuC_lx0DJ7
[<Literal>]
let internal parseRegexPattern = "
(?<double_open>\{\{)
|
(?<double_close>\}\})
|
(?<fullprop>
 (?<start_prop>\{)
 (?<destr>\@|\$)?
 (?<property>[\w\.\[\]]+)
 (?<align>,-?[\d]+)?
 (?<format>:[^}]+)?
 (?<end_prop>\})+
)
|
(?<other_text>\{?[^\{]+)
"

let private parseRegex =
    System.Text.RegularExpressions.Regex(parseRegexPattern, regexOptions)

let tryParseInt32 s =
    System.Int32.TryParse(s, System.Globalization.NumberStyles.None, System.Globalization.CultureInfo.InvariantCulture)

let maybeMapPropertyInfo (m:Match) : PropertyData option =
    let g = m.Groups
    let isNullOrEmpty = System.String.IsNullOrEmpty
    let someIfSuccessAndNotEmpty (m:Group) =
        if m.Success && not (isNullOrEmpty m.Value) then Some (m.Value.TrimStart(':'))
        else None
    if g.[GRP.DOUBLE_OPEN].Success || g.[GRP.DOUBLE_CLOSE].Success then
        None
    else if not (g.[GRP.FULL_PROP].Success) then
        None
    else
        let destrGroup = g.[GRP.DESTR]
        let ag = g.[GRP.ALIGN]
        match ag.Value with
        | ",-0" | ",0" -> None
        | _ ->
            let agWithoutComma = if ag.Value.Length > 0 then (ag.Value.TrimStart(',')) else ""
            let agFirstNumChar = if agWithoutComma.Length > 0 then agWithoutComma.Chars(0) else ' '

            let propName = g.[GRP.PROPERTY].Value
            
            Some { Name = propName
                   Pos = match tryParseInt32 propName with true, p -> Some p | _ -> None
                   Destr = match destrGroup.Success, destrGroup.Value with
                           | true, "@" -> DestructureKind.Destructure
                           | true, "$" -> DestructureKind.Stringify
                           | _, _ -> DestructureKind.Default
                   Align = match ag.Success, agFirstNumChar, agWithoutComma  with
                           | true, '-', num -> Some { Direction = Direction.Right; Width = int num; }
                           | true, _, num -> Some { Direction = Direction.Left; Width = int num; }
                           | false, _, _ -> None
                   Format = someIfSuccessAndNotEmpty g.[GRP.FORMAT] }

let private parseTokens (s: string) : Token seq =
    if s = "" then Seq.singleton (Text({ Text = ""; StartIndex = 0; }))
    else
        let matches = parseRegex.Matches(s)
        if matches.Count = 0 then Seq.empty
        else seq {
            for m in matches do
                let pi = maybeMapPropertyInfo m
                let tkInfo = { Text = m.Value.Replace("{{", "{"); StartIndex = m.Index }
                if pi.IsSome then
                    yield Prop(tkInfo, pi.Value)
                else
                    yield Text(tkInfo)
        }

// Try this: > MessageTemplates.parse "wh {{ {0:abc} at the {@heck} #{$align,11:0}?";;

let parse (s:string) =
    { FormatString = s; Tokens = s |> parseTokens |> List.ofSeq }

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
        | Prop (_, pi) -> sprintf "{%i%s%s}" (getNextPropPos()) (alignToFormat pi.Align) (formatOrEmpty pi.Format)

    let nextPos =
        let i = ref -1
        fun () -> i := !i + 1; !i

    let strings = t.Tokens |> Seq.map (tokenToPositionalFormat nextPos) |> Seq.toArray
    System.String.Join("", strings)

let format provider template values =
    System.String.Format(provider, createPositionalFormat template, values)


