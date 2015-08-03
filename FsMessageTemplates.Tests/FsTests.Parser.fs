module FsTests.Parser

open Xunit
open Swensen.Unquote.Assertions
open System.Globalization
open System

(*
    These tests run against both the C# and F# version. This helps to maintain the same
    behaviour in both implementations.
*)

type FsToken = FsMessageTemplates.MessageTemplates.Token

let assertParsedAs lang message (expectedTokens: System.Collections.IEnumerable) =
    let parsed =
        match lang with
        | "C#" -> MessageTemplates.MessageTemplate.Parse(message).Tokens |> Seq.map CsToFs.mttToToken |> List.ofSeq
        | "F#" -> (FsMessageTemplates.MessageTemplates.parse message).Tokens
        | other -> failwithf "unexpected lang '%s'" other

    let expected = expectedTokens |> Seq.cast<FsToken> |> Seq.toList
    test <@ parsed = expected @>

[<LangTheory; LangCsFsData>]
let ``an empty message is a single text token`` (lang) = 
    assertParsedAs lang  "" [Tk.text 0 ""]

[<LangTheory; LangCsFsData>]
let ``a message without properties is a single text token`` (lang) =
    assertParsedAs lang  "Hello, world!"
                   [Tk.text 0 "Hello, world!"]

[<LangTheory; LangCsFsData>]
let ``a message with property only is a single property token`` (lang) =
    let template = "{Hello}"
    assertParsedAs lang template
                   [Tk.prop 0 template "Hello"]

[<LangTheory; LangCsFsData>]
let ``doubled left brackets are parsed as a single bracket`` (lang) =
    let template = "{{ Hi! }"
    assertParsedAs lang template
                   [Tk.text 0 "{ Hi! }"]

[<LangTheory; LangCsFsData>]
let ``doubled left brackets are parsed as a single bracket inside text`` (lang) =
    let template = "Well, {{ Hi!"
    assertParsedAs lang template
                   [Tk.text 0 "Well, { Hi!"]

[<LangTheory; LangCsFsData>]
let ``doubled left and right brackets inside a property are parsed as text`` (lang) =
    let template = "Hello, {nam{{adam}}}, how's it going?"
    assertParsedAs lang template
                   // This is what I think *should* be returned: [Tk.text 0 "Hello, {nam{adam}}, how's it going?"]
                   // This is what is *actually* returned:
                   [Tk.text 0 "Hello, " // <- Arguably, there should be a single text token for the whole input
                    Tk.text 7 "{nam{{adam}" // <- the double-braces are "wrong", does anyone care?
                    Tk.text 18 "}, how's it going?"]

[<LangTheory; LangCsFsData>]
let ``parsing 100k performance`` (lang) =
    let template = "Hello, {nam{{adam}}}, how's {whatever,-10:00,0} it going?"
    
    let test =
        match lang with
        | "C#" -> fun s -> MessageTemplates.MessageTemplate.Parse s |> ignore
        | "F#" -> fun s -> FsMessageTemplates.MessageTemplates.parse s |> ignore
        | _ -> failwithf "unexpected lang %s" lang

    for _ in 1..100000 do test template

[<LangTheory; LangCsFsData>]
let ``doubled right brackets are parsed as a single bracket`` (lang) =
    let template = "Nice }}-: mo"
    assertParsedAs lang template
                   [Tk.text 0 "Nice }-: mo"]

[<LangTheory; LangCsFsData>]
let ``a malformed property tag is parsed as text`` (lang) =
    let template = "{0 space}"
    assertParsedAs lang template
                   [Tk.text 0 template]

[<LangTheory; LangCsFsData>]
let ``an integer property name is parsed as positional property`` (lang) =
    let template = "{0} text {1} other text {2}"
    assertParsedAs lang template
                   [Tk.propp 0 0
                    Tk.text 3 " text "
                    Tk.propp 9 1
                    Tk.text 12 " other text "
                    Tk.propp 24 2]

[<LangTheory; LangCsFsData>]
let ``formats can contain colons`` (lang) =
    let template = "{Time:hh:mm}"
    assertParsedAs lang template
                   [Tk.propf 0 template "Time" "hh:mm" ]

[<LangTheory; LangCsFsData>]
let ``formats can contain commas`` (lang) = 
    let template = ",{CustomerId:0,0},"
    assertParsedAs lang template
                   [Tk.text 0 ","
                    Tk.propf 1 "{CustomerId:0,0}" "CustomerId" "0,0"
                    Tk.text 17 ","]

[<LangTheory; LangCsFsData>]
let ``zero values alignment is parsed as text`` (lang) =
    let template1 = "{Hello,-0}"
    assertParsedAs lang template1
                   [Tk.text 0 template1]

    let template2 = "{Hello,0}"
    assertParsedAs lang template2
                   [Tk.text 0 template2]
 
 
[<LangTheory; LangCsFsData>]
let ``non-number alignment is parsed as text`` (lang) =
    let t1 = "{Hello,-aa}"
    assertParsedAs lang t1 [Tk.text 0 t1]

    let t2 = "{Hello,aa}"
    assertParsedAs lang t2 [Tk.text 0 t2]

    let t3 = "{Hello,-10-1}"
    assertParsedAs lang t3 [Tk.text 0 t3]

    let t4 = "{Hello,10-1}"
    assertParsedAs lang t4 [Tk.text 0 t4]

[<LangTheory; LangCsFsData>]
let ``empty alignment is parsed as text`` (lang) =
    let t1 = "{Hello,}"
    assertParsedAs lang t1 [Tk.text 0 t1]

    let t2 = "{Hello,:format}"
    assertParsedAs lang t2 [Tk.text 0 t2]

[<LangTheory; LangCsFsData>]
let ``multiple tokens have the correct indexes`` (lang) =
    let template = "{Greeting}, {Name}!"
    assertParsedAs lang template
                   [Tk.prop 0 "{Greeting}" "Greeting"
                    Tk.text 10 ", "
                    Tk.prop 12 "{Name}" "Name"
                    Tk.text 18 "!" ]

[<LangTheory; LangCsFsData>]
let ``missing right bracket is parsed as text`` (lang) =
    let template = "{Hello"
    assertParsedAs lang template [Tk.text 0 template]

[<LangTheory; LangCsFsData>]
let ``destructure hint is parsed correctly`` (lang) =
    let template = "{@Hello}"
    assertParsedAs lang template [Tk.propd 0 template "Hello"]

[<LangTheory; LangCsFsData>]
let ``stringify hint is parsed correctly`` (lang) =
    let template = "{$Hello}"
    assertParsedAs lang template [Tk.propds 0 template "Hello"]

[<LangTheory; LangCsFsData>]
let ``destructuring with empty property name is parsed as text`` (lang) =
    let template = "{@}"
    assertParsedAs lang template [Tk.text 0 template]
    
[<LangTheory; LangCsFsData>]
let ``underscores are valid in property names`` (lang) =
    assertParsedAs lang  "{_123_Hello}" [Tk.prop 0 "{_123_Hello}" "_123_Hello"]

[<LangTheory; LangCsFsData>]
let ``multiple properties use format provider`` (lang) =
    let m = renderp lang (CultureInfo.GetCultureInfo "fr-FR")
                    "Income was {Income} at {Date:d}" [box 1234.567; box (DateTime(2013, 5, 20))]
    test <@ m = "Income was 1234,567 at 20/05/2013" @>

[<LangTheory; LangCsFsData>]
let ``format strings are propagated`` (lang) =
    let m = render lang "Welcome, customer {CustomerId:0000}" [12]
    test <@ m = "Welcome, customer 0012" @>

[<LangTheory>]
[<InlineData("C#", "Welcome, customer #{CustomerId,-10}, pleasure to see you", "Welcome, customer #1234      , pleasure to see you")>]
[<InlineData("C#", "Welcome, customer #{CustomerId,-10:000000}, pleasure to see you", "Welcome, customer #001234    , pleasure to see you")>]
[<InlineData("C#", "Welcome, customer #{CustomerId,10}, pleasure to see you", "Welcome, customer #      1234, pleasure to see you")>]
[<InlineData("C#", "Welcome, customer #{CustomerId,10:000000}, pleasure to see you", "Welcome, customer #    001234, pleasure to see you")>]
[<InlineData("C#", "Welcome, customer #{CustomerId,10:0,0}, pleasure to see you", "Welcome, customer #     1,234, pleasure to see you")>]
[<InlineData("C#", "Welcome, customer #{CustomerId:0,0}, pleasure to see you", "Welcome, customer #1,234, pleasure to see you")>]
[<InlineData("F#", "Welcome, customer #{CustomerId,-10}, pleasure to see you", "Welcome, customer #1234      , pleasure to see you")>]
[<InlineData("F#", "Welcome, customer #{CustomerId,-10:000000}, pleasure to see you", "Welcome, customer #001234    , pleasure to see you")>]
[<InlineData("F#", "Welcome, customer #{CustomerId,10}, pleasure to see you", "Welcome, customer #      1234, pleasure to see you")>]
[<InlineData("F#", "Welcome, customer #{CustomerId,10:000000}, pleasure to see you", "Welcome, customer #    001234, pleasure to see you")>]
[<InlineData("F#", "Welcome, customer #{CustomerId,10:0,0}, pleasure to see you", "Welcome, customer #     1,234, pleasure to see you")>]
[<InlineData("F#", "Welcome, customer #{CustomerId:0,0}, pleasure to see you", "Welcome, customer #1,234, pleasure to see you")>]
let ```alignment strings are propagated`` (lang:string) (template:string) (expected:string) =
    let m = render lang template [1234]
    test <@ m = expected @>

[<LangTheory; LangCsFsData>]
let ``format provider is used`` (lang) =
    let m = renderp lang (CultureInfo.GetCultureInfo "fr-FR")
                   "Please pay {Sum}" [12.345]
    test <@ m = "Please pay 12,345" @>

