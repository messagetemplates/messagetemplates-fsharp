module FsTests.Format

open System
open System.Globalization
open Swensen.Unquote
open Xunit

type Chair() =
    member __.Back with get() = "straight"
    member __.Legs with get() = [|1;2;3;4|]
    override __.ToString() = "a chair"

type Receipt() =
    member __.Sum with get() = 12.345m
    member __.When with get() = System.DateTime(2013, 5, 20, 16, 39, 0)
    override __.ToString() = "a receipt"

[<LangTheory; LangCsFsData>]
let ``a class instance is rendered in simple notation`` (lang) =
    let m = render lang "I sat at {@Chair}" [Chair()]
    test <@ m = "I sat at Chair { Back: \"straight\", Legs: [1, 2, 3, 4] }" @>

[<LangTheory; LangCsFsData>]
let ``a class instance is rendered in simple notation using format provider`` (lang) =
    let m = renderp lang (CultureInfo.GetCultureInfo "fr-FR") "I received {@Receipt}" [Receipt()]
    test <@ m = "I received Receipt { Sum: 12,345, When: 20/05/2013 16:39:00 }" @>

type ChairRecord = { Back:string; Legs: int array }

[<LangTheory; LangCsFsData>]
let ``an F# record object is rendered in simple notation with type`` (lang) =
    let m = render lang "I sat at {@Chair}" [{ Back="straight"; Legs=[|1;2;3;4|] }]
    test <@ m = "I sat at ChairRecord { Back: \"straight\", Legs: [1, 2, 3, 4] }" @>

type ReceiptRecord = { Sum: double; When: System.DateTime }

[<LangTheory; LangCsFsData>]
let ``an F# record object is rendered in simple notation with type using format provider`` (lang) =
    let m = renderp lang (CultureInfo.GetCultureInfo "fr-FR")
                    "I received {@Receipt}" [{ Sum=12.345; When=DateTime(2013, 5, 20, 16, 39, 0) }]
    test <@ m = "I received ReceiptRecord { Sum: 12,345, When: 20/05/2013 16:39:00 }" @>

[<LangTheory; LangCsFsData>]
let ``an object with default destructuring is rendered as a string literal`` (lang) =
    let m = render lang "I sat at {Chair}" [Chair()]
    test <@ m = "I sat at \"a chair\"" @>

[<LangTheory; LangCsFsData>]
let ``an object with stringify destructuring is rendered as a string`` (lang) =
    let m = render lang "I sat at {$Chair}" [Chair()]
    test <@ m = "I sat at \"a chair\"" @>

[<LangTheory; LangCsFsData>]
let ``multiple properties are rendered in order`` (lang) =
    let m = render lang "Just biting {Fruit} number {Count}" [box "Apple"; box 12]
    test <@ m = "Just biting \"Apple\" number 12" @>
    
[<LangTheory; LangCsFsData>]
let ``a template with only positional properties is analyzed and rendered positionally`` (lang) =
    let m = render lang "{1}, {0}" ["world"; "Hello"]
    test <@ m = "\"Hello\", \"world\"" @>

[<LangTheory; LangCsFsData>]
let ``a template with only positional properties uses format provider`` (lang) =
    let m = renderp lang (CultureInfo.GetCultureInfo "fr-FR")
                    "{1}, {0}" [box 12.345; box "Hello"]
    test <@ m = "\"Hello\", 12,345" @>

// Debatable what the behavior should be, here.
[<LangTheory; LangCsFsData>]
let ``a template with names and positionals uses names for all values`` (lang) =
    let m = render lang "{1}, {Place}" ["world"; "Hello"]
    test <@ m = "\"world\", \"Hello\"" @>

[<LangTheory; LangCsFsData>]
let ``missing positional parameters render as text like standard formats`` (lang) =
    let m = render lang "{1}, {0}" ["world"]
    test <@ m = "{1}, \"world\"" @>

    
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

