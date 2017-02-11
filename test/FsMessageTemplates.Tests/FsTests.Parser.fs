module FsTests.Parser

open Xunit
open System.Globalization
open System
open FsMessageTemplates

(*
    These tests run against both the C# and F# version. This helps to maintain the same
    behaviour in both implementations.
*)
let assertParsedAs lang (message: string) expectedTokens =
    FsTests.Asserts.MtAssert.ParsedAs(lang, message, expectedTokens)

[<Fact>]
let ``FsMtParser does something without crashing`` () =
  let foundText t = ()
  let foundProp p = ()
  FsMtParser.parseParts "Hello {adam:#0.000}, how are {you? you crazy invalid prop}" foundText foundProp

[<Fact>]
let ``align info defaults are correct`` () =
    Assert.Equal (FsMessageTemplates.Direction.Left, AlignInfo().Direction)
    Assert.Equal (0, AlignInfo().Width)
    Assert.Equal (false, AlignInfo().IsEmpty)

[<LangTheory; CSharpAndFSharp>]
let ``an empty message is a single text token`` (lang) = 
    assertParsedAs lang  "" [Tk.text 0 ""]

[<LangTheory; AllImplementations>]
let ``a message without properties is a single text token`` (lang) =
    assertParsedAs lang  "Hello, world!"
                   [Tk.text 0 "Hello, world!"]

[<LangTheory; AllImplementations>]
let ``a message with property only is a single property token`` (lang) =
    let template = "{Hello}"
    assertParsedAs lang template
                   [Tk.prop 0 template "Hello"]

[<LangTheory; AllImplementations>]
let ``doubled left brackets are parsed as a single bracket`` (lang) =
    let template = "{{ Hi! }"
    assertParsedAs lang template
                   [Tk.text 0 "{ Hi! }"]

[<LangTheory; AllImplementations>]
let ``doubled left brackets are parsed as a single bracket inside text`` (lang) =
    let template = "Well, {{ Hi!"
    assertParsedAs lang template
                   [Tk.text 0 "Well, { Hi!"]

[<LangTheory; AllImplementations>]
let ``doubled left and right brackets inside a property are parsed as text`` (lang) =
    let template = "Hello, {nam{{adam}}}, how's it going?"
    assertParsedAs lang template
                   // This is what I think *should* be returned: [Tk.text 0 "Hello, {nam{adam}}, how's it going?"]
                   // This is what is *actually* returned:
                   [Tk.text 0 "Hello, " // <- Arguably, there should be a single text token for the whole input
                    Tk.text 7 "{nam{{adam}" // <- the double-braces are "wrong", does anyone care?
                    Tk.text 18 "}, how's it going?"]

[<LangTheory; AllImplementations>]
let ``doubled right brackets are parsed as a single bracket`` (lang) =
    let template = "Nice }}-: mo"
    assertParsedAs lang template
                   [Tk.text 0 "Nice }-: mo"]

[<LangTheory; AllImplementations>]
let ``a malformed property tag is parsed as text`` (lang) =
    let template = "{0 space}"
    assertParsedAs lang template
                   [Tk.text 0 template]

[<LangTheory; CSharpAndFSharp>]
let ``an integer property name is parsed as positional property`` (lang) =
    let template = "{0} text {1} other text {2}"
    assertParsedAs lang template
                   [Tk.propp 0 0
                    Tk.text 3 " text "
                    Tk.propp 9 1
                    Tk.text 12 " other text "
                    Tk.propp 24 2]

[<LangTheory; AllImplementations>]
let ``formats can contain colons`` (lang) =
    let template = "{Time:hh:mm}"
    assertParsedAs lang template
                   [Tk.propf 0 template "Time" "hh:mm" ]

[<LangTheory; AllImplementations>]
let ``formats can contain commas`` (lang) = 
    let template = ",{CustomerId:0,0},"
    assertParsedAs lang template
                   [Tk.text 0 ","
                    Tk.propf 1 "{CustomerId:0,0}" "CustomerId" "0,0"
                    Tk.text 17 ","]
                    
[<LangTheory; CSharpAndFSharp>]
let ``formats with align right can contain commas`` (lang) = 
    let template = "big long x{0,5:0,00}x{1,5:0,00}x{2,5:0,00}x"
    assertParsedAs lang template
                   [ Tk.text 0 "big long x"
                     Tk.propparf 10 0 5 "0,00"
                     Tk.text 20 "x"
                     Tk.propparf 21 1 5 "0,00"
                     Tk.text 31 "x"
                     Tk.propparf 32 2 5 "0,00"
                     Tk.text 42 "x"]

[<LangTheory; CSharpAndFSharp>]
let ``formats with align left can contain commas`` (lang) = 
    let template = "big long x{0,-5:0,00}x{1,-5:0,00}x{2,-5:0,00}x"
    assertParsedAs  lang template
                    [ Tk.text 0 "big long x"
                      Tk.proppalf 10 0 5 "0,00"
                      Tk.text 21 "x"
                      Tk.proppalf 22 1 5 "0,00"
                      Tk.text 33 "x"
                      Tk.proppalf 34 2 5 "0,00"
                      Tk.text 45 "x" ]


[<LangTheory; AllImplementations>]
let ``zero values alignment is parsed as text`` (lang) =
    let template1 = "{Hello,-0}"
    assertParsedAs lang template1
                   [Tk.text 0 template1]

    let template2 = "{Hello,0}"
    assertParsedAs lang template2
                   [Tk.text 0 template2]
 
 
[<LangTheory; AllImplementations>]
let ``non-number alignment is parsed as text`` (lang) =
    let t1 = "{Hello,-aa}"
    assertParsedAs lang t1 [Tk.text 0 t1]

    let t2 = "{Hello,aa}"
    assertParsedAs lang t2 [Tk.text 0 t2]

    let t3 = "{Hello,-10-1}"
    assertParsedAs lang t3 [Tk.text 0 t3]

    let t4 = "{Hello,10-1}"
    assertParsedAs lang t4 [Tk.text 0 t4]

[<LangTheory; AllImplementations>]
let ``empty alignment is parsed as text`` (lang) =
    let t1 = "{Hello,}"
    assertParsedAs lang t1 [Tk.text 0 t1]

    let t2 = "{Hello,:format}"
    assertParsedAs lang t2 [Tk.text 0 t2]

[<LangTheory; CSharpAndFSharp>]
let ``multiple tokens have the correct indexes`` (lang) =
    let template = "{Greeting}, {Name}!"
    assertParsedAs lang template
                   [Tk.prop 0 "{Greeting}" "Greeting"
                    Tk.text 10 ", "
                    Tk.prop 12 "{Name}" "Name"
                    Tk.text 18 "!" ]

[<LangTheory; AllImplementations>]
let ``missing right bracket is parsed as text`` (lang) =
    let template = "{Hello"
    assertParsedAs lang template [Tk.text 0 template]

[<LangTheory; FullParserImplementations>]
let ``destructure hint is parsed correctly`` (lang) =
    let template = "{@Hello}"
    assertParsedAs lang template [Tk.propd 0 template "Hello"]

[<LangTheory; FullParserImplementations>]
let ``stringify hint is parsed correctly`` (lang) =
    let template = "{$Hello}"
    assertParsedAs lang template [Tk.propds 0 template "Hello"]

[<LangTheory; FullParserImplementations>]
let ``destructuring with empty property name is parsed as text`` (lang) =
    let template = "{@}"
    assertParsedAs lang template [Tk.text 0 template]
    
[<LangTheory; AllImplementations>]
let ``underscores are valid in property names`` (lang) =
    assertParsedAs lang  "{_123_Hello}" [Tk.prop 0 "{_123_Hello}" "_123_Hello"]

//[<LangTheory; CSharpAndFSharp>]
//let ``property names starting with zero are positional`` (lang) =
//    assertParsedAs lang  "{0001}" [Tk.propp 0 1]

