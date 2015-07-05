
module MessageTemplates =
    open System.Text.RegularExpressions

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

    type PropertyToken = {
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

    type Template = {
        FormatString: string
        Properties: PropertyToken list }

    let private regexOptions = RegexOptions.Compiled ||| RegexOptions.IgnorePatternWhitespace
    
    [<Literal>]
    let startGrp = "start"

    let private extractPropertiesRegex = Regex("([^{] 
 (?<start_prop>\{)
 (?<destr>\@|\$)?
 (?<property>[\w\.\[\]]+)
 (?<align>,[^:}]+)?
 (?<format>:[^}]+)?
 (?<end_prop>\})+)
|
(?<double_open>\{\{)
|
(?<double_close>\}\})", regexOptions)

    let private parseProperties (s: string) =
        let matchToProperty (m:Match) : PropertyToken option =
            let isNullOrEmpty = System.String.IsNullOrEmpty
            let someIfSuccessAndNotEmpty (m:Group) =
                if m.Success && not (isNullOrEmpty m.Value) then Some m.Value
                else None

            if m.Groups.["double_open"].Success
               || m.Groups.["double_close"].Success then None
            else Some { Name = m.Groups.["property"].Value
                        DestructuringHint = Destructure.FromGroup m.Groups.["destr"]
                        Align = someIfSuccessAndNotEmpty m.Groups.["align"]
                        Format = someIfSuccessAndNotEmpty m.Groups.["format"]
                        Index = m.Index + 1 } // +1 because our regex has to capture the previous char too because of '{{'

        let matches = extractPropertiesRegex.Matches(s)
        if matches.Count = 0 then
            Seq.empty
        else
            matches
            |> Seq.cast<Match>
            |> Seq.map matchToProperty

    // Try this: > MessageTemplates.parse "wh {{ {0:abc} at the {@heck} #{$align,11:0}?";;

    let parse (s:string) = 
        { FormatString = s
          Properties = (parseProperties s) |> Seq.choose id |> List.ofSeq
        }

    let format (t: Template) ([<System.ParamArray>] values : 'a array) = ""


