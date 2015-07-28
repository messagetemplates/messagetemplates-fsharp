module FsTests

open Xunit
open FsMessageTemplates.MessageTemplates

open MessageTemplates.Parsing
let (|Null|Value|) (x: _ System.Nullable) = if x.HasValue then Value x.Value else Null
let textToToken (tt: TextToken) = Token.Text({ StartIndex=tt.StartIndex; Text=tt.Text })
let propToToken (pr: PropertyToken) =
    let pos = match pr.TryGetPositionalValue() with
              | true, i -> Some i
              | false, _ -> None
    let destr = match pr.Destructuring with
                | Destructuring.Default -> DestructureKind.Default
                | Destructuring.Destructure -> DestructureKind.Destructure
                | Destructuring.Stringify -> DestructureKind.Stringify
                | d -> failwithf "unknown destructure %A" d
    let getDirection d = match d with
                         | AlignmentDirection.Left -> Direction.Left
                         | AlignmentDirection.Right -> Direction.Right
                         | _ -> failwithf "unknown direction %A" d
    let align = match pr.Alignment with
                | Value v -> Some { Direction = (getDirection v.Direction)
                                    Width = v.Width }
                | Null _ -> None
    let format = match pr.Format with | null -> None | s -> Some s
    Token.Prop({ StartIndex=pr.StartIndex; Text=pr.ToString() },
               { Name=pr.PropertyName; Pos=pos; Destr=destr; Align=align; Format=format; })

let mttToToken (mtt: MessageTemplateToken) : Token =
    match mtt with
    | :? PropertyToken as pt -> propToToken pt
    | :? TextToken as tt -> textToToken tt
    | _ -> failwithf "unknown token %A" mtt

open Swensen.Unquote.Assertions
open System.Globalization
open System

type LangTheoryAttribute() =
    inherit Xunit.TheoryAttribute()
type LangCsFsDataAttribute() =
    inherit Xunit.Sdk.DataAttribute()
    override __.GetData _ = [[|box "C#"|]; [|box "F#"|]] |> Seq.ofList

let assertParsedAs lang message (expectedTokens: System.Collections.IEnumerable) =
    let parsed =
        match lang with
        | "C#" -> MessageTemplates.MessageTemplate.Parse(message).Tokens
                  |> Seq.map mttToToken |> List.ofSeq
        | "F#" -> (FsMessageTemplates.MessageTemplates.parse message).Tokens
        | other -> failwithf "unexpected lang '%s'" other

    let expected = expectedTokens |> Seq.cast<Token> |> Seq.toList
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
                   [Tk.text 0 "Hello, " // <- Arguably, there should be a single text token for the whole input
                    Tk.text 7 "{nam{{adam}" //<- the double-braces are "wrong"
                    Tk.text 18 "}, how's it going?"]

[<LangTheory; LangCsFsData>]
let ``performance is good`` (lang) =
    let template = "Hello, {nam{{adam}}}, how's {whatever,-10:00,0} it going?"
    
    let test =
        match lang with
        | "C#" -> fun s -> MessageTemplates.MessageTemplate.Parse s |> ignore
        | "F#" -> fun s -> FsMessageTemplates.MessageTemplates.parse s |> ignore
        | s -> failwithf "unexpected lang %s" s

    for i in 1..100000 do
        test(template)


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

let renderp lang (provider:IFormatProvider) messageTemplate args =
    let argsArray = (args |> Seq.cast<obj> |> Seq.toArray) // force 'args' to be IEnumerable
    match lang with
    | "C#" -> MessageTemplates.MessageTemplate.Format(provider, messageTemplate, argsArray)
    | "F#" -> FsMessageTemplates.MessageTemplates.format provider
                                                        (FsMessageTemplates.MessageTemplates.parse messageTemplate)
                                                        argsArray
    | other -> failwithf "unexpected lang '%s'" other

let render lang template args =
    renderp lang CultureInfo.InvariantCulture template args

type Chair() =
    member __.Back with get() = "straight"
    member __.Legs with get() = [|1;2;3;4|]
    override __.ToString() = "a chair"

type Receipt() =
    member __.Sum with get() = 12.345m
    member __.When with get() = System.DateTime(2013, 5, 20, 16, 39, 0)
    override __.ToString() = "a receipt"

[<LangTheory(Skip="Destructure formatting not implemented in F# yet"); LangCsFsData>]
let ``a class instance is rendered in simple notation`` (lang) =
    let m = render lang "I sat at {@Chair}" [Chair()]
    test <@ m = "I sat at Chair { Back: \"straight\", Legs: [1, 2, 3, 4] }" @>

[<LangTheory(Skip="Destructure formatting not implemented in F# yet"); LangCsFsData>]
let ``a class instance is rendered in simple notation using format provider`` (lang) =
    let m = renderp lang (CultureInfo.GetCultureInfo "fr-FR") "I received {@Receipt}" [Receipt()]
    test <@ m = "I received Receipt { Sum: 12,345, When: 20/05/2013 16:39:00 }" @>

type ChairRecord = { Back:string; Legs: int array }

[<LangTheory(Skip="Destructure formatting not implemented in F# yet"); LangCsFsData>]
let ``an F# record object is rendered in simple notation with type`` (lang) =
    let m = render lang "I sat at {@Chair}" [{ Back="straight"; Legs=[|1;2;3;4|] }]
    test <@ m = "I sat at ChairRecord { Back: \"straight\", Legs: [1, 2, 3, 4] }" @>

type ReceiptRecord = { Sum: double; When: System.DateTime }

[<LangTheory(Skip="Destructure formatting not implemented in F# yet"); LangCsFsData>]
let ``an F# record object is rendered in simple notation with type using format provider`` (lang) =
    let m = renderp lang (CultureInfo.GetCultureInfo "fr-FR")
                    "I received {@Receipt}" [{ Sum=12.345; When=DateTime(2013, 5, 20, 16, 39, 0) }]
    test <@ m = "I received ReceiptRecord { Sum: 12,345, When: 20/05/2013 16:39:00 }" @>

[<LangTheory(Skip="Destructure formatting not implemented in F# yet"); LangCsFsData>]
let ``an object with default destructuring is rendered as a string literal`` (lang) =
    let m = render lang "I sat at {Chair}" [Chair()]
    test <@ m = "I sat at \"a chair\"" @>

[<LangTheory(Skip="Destructure formatting not implemented in F# yet"); LangCsFsData>]
let ``an object with stringify destructuring is rendered as a string`` (lang) =
    let m = render lang "I sat at {$Chair}" [Chair()]
    test <@ m = "I sat at \"a chair\"" @>

[<LangTheory(Skip="Destructure formatting not implemented in F# yet"); LangCsFsData>]
let ``multiple properties are rendered in order`` (lang) =
    let m = render lang "Just biting {Fruit} number {Count}" [box "Apple"; box 12]
    test <@ m = "Just biting \"Apple\" number 12" @>

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


[<LangTheory(Skip="Destructure formatting not implemented in F# yet"); LangCsFsData>]
let ``a template with only positional properties is analyzed and rendered positionally`` (lang) =
    let m = render lang "{1}, {0}" ["world"; "Hello"]
    test <@ m = "\"Hello\", \"world\"" @>

[<LangTheory(Skip="Destructure formatting not implemented in F# yet"); LangCsFsData>]
let ``a template with only positional properties uses format provider`` (lang) =
    let m = renderp lang (CultureInfo.GetCultureInfo "fr-FR")
                    "{1}, {0}" [box 12.345; box "Hello"]
    test <@ m = "\"Hello\", 12,345" @>

// Debatable what the behavior should be, here.
[<LangTheory(Skip="Destructure formatting not implemented in F# yet"); LangCsFsData>]
let ``a template with names and positionals uses names for all values`` (lang) =
    let m = render lang "{1}, {Place}" ["world"; "Hello"]
    test <@ m = "\"world\", \"Hello\"" @>

[<LangTheory(Skip="Destructure formatting not implemented in F# yet"); LangCsFsData>]
let ``missing positional parameters render as text like standard formats`` (lang) =
    let m = render lang "{1}, {0}" ["world"]
    test <@ m = "{1}, \"world\"" @>


