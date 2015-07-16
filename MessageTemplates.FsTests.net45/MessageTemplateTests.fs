module FsTests

open Xunit
open MessageTemplates.Parsing
open FsMessageTemplates.MessageTemplates

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

let parse (message: string) =
    let parser = MessageTemplates.Parsing.MessageTemplateParser()
    let template = parser.Parse(message)
    template.Tokens |> Seq.map mttToToken |> Seq.toArray

let tokensToString tokens = 
    let strings = tokens |> Seq.cast<Token> |> Seq.map (function | Text t -> t.ToString() | Prop (_, p) -> p.ToString()) |> Seq.toArray
    System.String.Join("\n", strings)


let test = Swensen.Unquote.Assertions.test

let assertParsedAs message (expectedTokens: System.Collections.IEnumerable) =
    let parsed = parse message;
    let expected = expectedTokens |> Seq.cast<Token> |> Seq.toArray
    test <@ parsed = expected @>

[<Fact>]
let ``an empty message is a single text token`` () = 
    assertParsedAs "" [Tk.text 0 ""]

[<Fact>]
let ``a message without properties is a single text token`` () =
    assertParsedAs "Hello, world!"
                   [Tk.text 0 "Hello, world!"]

[<Fact>]
let ``a message with property only is a single property token`` () =
    let template = "{Hello}"
    assertParsedAs template
                   [Tk.prop 0 template "Hello"]

[<Fact>]
let ``doubled left brackets are parsed as a single bracket`` () =
    let template = "{{ Hi! }"
    assertParsedAs template
                   [Tk.text 0 "{ Hi! }"]

[<Fact>]
let ``doubled left brackets are parsed as a single bracket inside text`` () =
    let template = "Well, {{ Hi!"
    assertParsedAs template
                   [Tk.text 0 "Well, { Hi!"]

[<Fact>]
let ``doubled right brackets are parsed as a single bracket`` () =
    let template = "Nice }}-: mo"
    assertParsedAs template
                   [Tk.text 0 "Nice }-: mo"]

[<Fact>]
let ``a malformed property tag is parsed as text`` () =
    let template = "{0 space}"
    assertParsedAs template
                   [Tk.text 0 template]

[<Fact>]
let ``an integer property name is parsed as positional property`` () =
    let template = "{0}"
    assertParsedAs template
                   [Tk.propp 0 0]
    
[<Fact>]
let ``formats can contain colons`` () =
    let template = "{Time:hh:mm}"
    assertParsedAs template
                   [Tk.propf 0 template "Time" "hh:mm" ]

[<Fact>]
let ``zero values alignment is parsed as text`` () =
    let template1 = "{Hello,-0}"
    assertParsedAs template1
                   [Tk.text 0 template1]

    let template2 = "{Hello,0}"
    assertParsedAs template2
                   [Tk.text 0 template2]
 
 
[<Fact>]
let ``non-number alignment is parsed as text`` () =
    let t1 = "{Hello,-aa}"
    assertParsedAs t1 [Tk.text 0 t1]

    let t2 = "{Hello,aa}"
    assertParsedAs t2 [Tk.text 0 t2]

    let t3 = "{Hello,-10-1}"
    assertParsedAs t3 [Tk.text 0 t3]

    let t4 = "{Hello,10-1}"
    assertParsedAs t4 [Tk.text 0 t4]

[<Fact>]
let ``empty alignment is parsed as text`` () =
    let t1 = "{Hello,}"
    assertParsedAs t1 [Tk.text 0 t1]

    let t2 = "{Hello,:format}"
    assertParsedAs t2 [Tk.text 0 t2]

[<Fact>]
let ``multiple tokens have the correct indexes`` () =
    let template = "{Greeting}, {Name}!"
    assertParsedAs template
                   [Tk.prop 0 "{Greeting}" "Greeting"
                    Tk.text 10 ", "
                    Tk.prop 12 "{Name}" "Name"
                    Tk.text 18 "!" ]

[<Fact>]
let ``missing right bracket is parsed as text`` () =
    let template = "{Hello"
    assertParsedAs template [Tk.text 0 template]

[<Fact>]
let ``destructure hint is parsed correctly`` () =
    let template = "{@Hello}"
    assertParsedAs template [Tk.propd 0 template "Hello"]

[<Fact>]
let ``stringify hint is parsed correctly`` () =
    let template = "{$Hello}"
    assertParsedAs template [Tk.propds 0 template "Hello"]

[<Fact>]
let ``destructuring with empty property name is parsed as text`` () =
    let template = "{@}"
    assertParsedAs template [Tk.text 0 template]
    
[<Fact>]
let ``underscores are valid in property names`` () =
    assertParsedAs "{_123_Hello}" [Tk.prop 0 "{_123_Hello}" "_123_Hello"]


(** TODO
class Chair
{
    // ReSharper disable UnusedMember.Local
    public string Back { get { return "straight"; } }
    public int[] Legs { get { return new[] { 1, 2, 3, 4 }; } }
    // ReSharper restore UnusedMember.Local
    public override string ToString()
    {
        return "a chair";
    }
}

class Receipt
{
    // ReSharper disable UnusedMember.Local
    public decimal Sum { get { return 12.345m; } }
    public DateTime When { get { return new DateTime(2013, 5, 20, 16, 39, 0); } }
    // ReSharper restore UnusedMember.Local
    public override string ToString()
    {
        return "a receipt";
    }
}

[Fact]
public void AnObjectIsRenderedInSimpleNotation()
{
    var m = Render("I sat at {@Chair}", new Chair());
    Assert.Equal("I sat at Chair { Back: \"straight\", Legs: [1, 2, 3, 4] }", m);
}

[Fact]
public void AnObjectIsRenderedInSimpleNotationUsingFormatProvider()
{
    var m = Render(CultureInfo.GetCultureInfo("fr-FR"), "I received {@Receipt}", new Receipt());
    Assert.Equal("I received Receipt { Sum: 12,345, When: 20/05/2013 16:39:00 }", m);
}

[Fact]
public void AnAnonymousObjectIsRenderedInSimpleNotationWithoutType()
{
    var m = Render("I sat at {@Chair}", new { Back = "straight", Legs = new[] { 1, 2, 3, 4 } });
    Assert.Equal("I sat at { Back: \"straight\", Legs: [1, 2, 3, 4] }", m);
}

[Fact]
public void AnAnonymousObjectIsRenderedInSimpleNotationWithoutTypeUsingFormatProvider()
{
    var m = Render(CultureInfo.GetCultureInfo("fr-FR"), "I received {@Receipt}", new { Sum = 12.345, When = new DateTime(2013, 5, 20, 16, 39, 0) });
    Assert.Equal("I received { Sum: 12,345, When: 20/05/2013 16:39:00 }", m);
}

[Fact]
public void AnObjectWithDefaultDestructuringIsRenderedAsAStringLiteral()
{
    var m = Render("I sat at {Chair}", new Chair());
    Assert.Equal("I sat at \"a chair\"", m);
}

[Fact]
public void AnObjectWithStringifyDestructuringIsRenderedAsAString()
{
    var m = Render("I sat at {$Chair}", new Chair());
    Assert.Equal("I sat at \"a chair\"", m);
}

[Fact]
public void MultiplePropertiesAreRenderedInOrder()
{
    var m = Render("Just biting {Fruit} number {Count}", "Apple", 12);
    Assert.Equal("Just biting \"Apple\" number 12", m);
}

[Fact]
public void MultiplePropertiesUseFormatProvider()
{
    var m = Render(CultureInfo.GetCultureInfo("fr-FR"), "Income was {Income} at {Date:d}", 1234.567, new DateTime(2013, 5, 20));
    Assert.Equal("Income was 1234,567 at 20/05/2013", m);
}

[Fact]
public void FormatStringsArePropagated()
{
    var m = Render("Welcome, customer {CustomerId:0000}", 12);
    Assert.Equal("Welcome, customer 0012", m);
}

[Theory]
[InlineData("Welcome, customer #{CustomerId,-10}, pleasure to see you", "Welcome, customer #1234      , pleasure to see you")]
[InlineData("Welcome, customer #{CustomerId,-10:000000}, pleasure to see you", "Welcome, customer #001234    , pleasure to see you")]
[InlineData("Welcome, customer #{CustomerId,10}, pleasure to see you", "Welcome, customer #      1234, pleasure to see you")]
[InlineData("Welcome, customer #{CustomerId,10:000000}, pleasure to see you", "Welcome, customer #    001234, pleasure to see you")]
[InlineData("Welcome, customer #{CustomerId,10:0,0}, pleasure to see you", "Welcome, customer #     1,234, pleasure to see you")]
[InlineData("Welcome, customer #{CustomerId:0,0}, pleasure to see you", "Welcome, customer #1,234, pleasure to see you")]
public void AlignmentStringsArePropagated(string value, string expected)
{
    Assert.Equal(expected, Render(value, 1234));
}

[Fact]
public void FormatProviderIsUsed()
{
    var m = Render(CultureInfo.GetCultureInfo("fr-FR"), "Please pay {Sum}", 12.345);
    Assert.Equal("Please pay 12,345", m);
}

static string Render(string messageTemplate, params object[] properties)
{
    return Render(null, messageTemplate, properties);
}

static string Render(IFormatProvider formatProvider,
    string messageTemplate, params object[] properties)
{
    var mt = new MessageTemplateParser().Parse(messageTemplate);
    var binder = new PropertyBinder(new PropertyValueConverter(10, Enumerable.Empty<Type>(), Enumerable.Empty<IDestructuringPolicy>()));
    var props = binder.ConstructProperties(mt, properties);
    var output = new StringBuilder();
    var writer = new StringWriter(output);
    var dict = System.Collections.Generic.Net40ReadOnlyDictionaryExtensions
        .ToDictionary40(props, p => p.Name, p => p.Value);
    mt.Render(dict, writer, formatProvider);
    writer.Flush();
    return output.ToString();
}

[Fact]
public void ATemplateWithOnlyPositionalPropertiesIsAnalyzedAndRenderedPositionally()
{
    var m = Render("{1}, {0}", "world", "Hello");
    Assert.Equal("\"Hello\", \"world\"", m);
}

[Fact]
public void ATemplateWithOnlyPositionalPropertiesUsesFormatProvider()
{
    var m = Render(CultureInfo.GetCultureInfo("fr-FR"), "{1}, {0}", 12.345, "Hello");
    Assert.Equal("\"Hello\", 12,345", m);
}

// Debatable what the behavior should be, here.
[Fact]
public void ATemplateWithNamesAndPositionalsUsesNamesForAllValues()
{
    var m = Render("{1}, {Place}", "world", "Hello");
    Assert.Equal("\"world\", \"Hello\"", m);
}

[Fact]
public void MissingPositionalParametersRenderAsTextLikeStandardFormats()
{
    var m = Render("{1}, {0}", "world");
    Assert.Equal("{1}, \"world\"", m);
}
**)