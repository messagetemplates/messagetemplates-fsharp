module FsMessageTemplates.MessageTemplates

open System.Text.RegularExpressions


let private maybeString s =
    match s with Some f -> f | None -> ""

type Destructure = Default | Destructure | Stringify
    with
        static member TryParse (s:string) =
            match s with
            | "@" -> Some Destructure
            | "$" -> Some Stringify
            | _ -> None
        static member FromGroup (g:Group) =
            match (g.Success, g.Value) with
            | false, _ | true, null | true, "" -> None
            | true, _ -> Destructure.TryParse g.Value

/// The index into the string where the first character of the token was found
type TextTokenInfo = {
    /// The literal text value.
    Text: string
    /// The index into the string where the first character of the token was found
    Index: int }
    with override x.ToString() = x.Text

type PropertyTokenInfo = {
    /// The property name extracted from the template message.
    Name: string
    /// The destructuring hint.
    DestructuringHint: Destructure option
    /// The alignment string (e.g. ",-10")
    Align: string option
    /// The 'format' string (e.g. ":0000")
    Format: string option
    /// The index into the string where the first character of the token was found
    Index: int }
    with override x.ToString() = sprintf "{%s%s%s%s}"
                                         (match x.DestructuringHint with Some d -> d.ToString() | None -> "")
                                         x.Name
                                         (maybeString x.Format)
                                         (maybeString x.Align)

type Token =
| TextToken of TextTokenInfo
| PropertyToken of PropertyTokenInfo
with override x.ToString() =
        match x with TextToken t -> t.ToString() | PropertyToken p -> p.ToString()

type Template = {
    FormatString: string
    Tokens: Token list }

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

let maybeMapPropertyInfo (m:Match) : PropertyTokenInfo option =
    let g = m.Groups
    let isNullOrEmpty = System.String.IsNullOrEmpty
    let someIfSuccessAndNotEmpty (m:Group) =
        if m.Success && not (isNullOrEmpty m.Value) then
            Some m.Value
        else
            None
    if g.[GRP.DOUBLE_OPEN].Success || g.[GRP.DOUBLE_CLOSE].Success then
        None
    else if not (g.[GRP.FULL_PROP].Success) then
        None
    else Some { Name = g.[GRP.PROPERTY].Value
                DestructuringHint = Destructure.FromGroup g.[GRP.DESTR]
                Align = someIfSuccessAndNotEmpty g.[GRP.ALIGN]
                Format = someIfSuccessAndNotEmpty g.[GRP.FORMAT]
                Index = m.Index }

let private parseTokens (s: string) : Token seq =
    let matches = parseRegex.Matches(s)
    if matches.Count = 0 then Seq.empty
    else seq {
        for m in matches do
            let pi = maybeMapPropertyInfo m
            if pi.IsSome then
                yield PropertyToken(pi.Value)
            else
                yield TextToken(
                    { TextTokenInfo.Text = m.Value
                      Index = m.Index })
    }

//let private parseProperties (s: string) =
//    let matches = parseRegex.Matches(s)
//    if matches.Count = 0 then
//        Seq.empty
//    else
//        matches
//        |> Seq.cast<Match>
//        |> Seq.map maybeMapPropertyInfo
//        |> Seq.choose id
//        |> Seq.map (fun pi -> PropertyToken(pi))

let parseTemplateString messageTemplate = parseTokens messageTemplate

// Try this: > MessageTemplates.parse "wh {{ {0:abc} at the {@heck} #{$align,11:0}?";;

let parse (s:string) =
    let props = (parseTemplateString s) |> List.ofSeq
    { FormatString = s; Tokens = props }

let createStringFormat (t: Template) =
    let mutable nextPropIndex = 0
    let tokenToPositional token = match token with
                    | TextToken ti -> ti.Text
                    | PropertyToken pi ->
                        let positionalFormat =
                            sprintf "{%i%s%s}"
                                nextPropIndex
                                (maybeString pi.Align)
                                (maybeString pi.Format)
                        nextPropIndex <- nextPropIndex + 1
                        positionalFormat

    let strings =
        t.Tokens |> Seq.map tokenToPositional |> Seq.toArray
    System.String.Join("", strings)

let format (provider: System.IFormatProvider)
           (t: Template)
           ([<System.ParamArray>] values : obj []) =
    let format = createStringFormat t
    System.String.Format(provider=provider, format=format, args=values)


