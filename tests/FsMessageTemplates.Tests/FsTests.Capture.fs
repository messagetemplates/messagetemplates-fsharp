module FsTests.Capture

open System
open Tk
open FsMessageTemplates
open Asserts

[<LangTheory; LangCsFsData>]
let ``no values provided yields no properties`` (lang) =
    MtAssert.DestructuredAs(lang, "this {will} {capture} {nothing}, I hope", [||], [])

[<LangTheory; LangCsFsData>]
let ``one named property and one value yields the correct named property`` (lang) =
    MtAssert.DestructuredAs(lang, "this {will} work, I hope", [|"might"|],
        expected = [ PropertyNameAndValue("will", ScalarValue "might") ])

[<LangTheory; LangCsFsData>]
let ``one enumerable property yeilds a sequence value`` (lang) =
    MtAssert.DestructuredAs(lang, "this {will} work, I hope", [|[|1..3|]|],
        expected = [ PropertyNameAndValue("will", SequenceValue
                        [ ScalarValue 1; ScalarValue 2; ScalarValue 3; ])])

type Chair() =
    member __.Back with get() = "straight"
    member __.Legs with get() = [|1;2;3;4|]
    override __.ToString() = "a chair"

let chairStructureValue =
    let propNamesAndValues = [ PropertyNameAndValue("Back", ScalarValue "straight")
                               PropertyNameAndValue("Legs", SequenceValue ([ ScalarValue 1; ScalarValue 2; ScalarValue 3; ScalarValue 4 ]))
                             ]
    StructureValue ("Chair", propNamesAndValues)

[<LangTheory; LangCsFsData>]
let ``a destructured dictionary yeilds dictionary values`` (lang) =
    let inputDictionary = System.Collections.Generic.Dictionary(dict [| "key", Chair(); |])
    MtAssert.DestructuredAs(lang, "this {@will} work, I hope", [| inputDictionary |],
        expected = [ PropertyNameAndValue("will", DictionaryValue [ ScalarValue "key", chairStructureValue ]) ])

[<LangTheory; LangCsFsData>]
let ``an F# 'dict' (which is not Dictionary<_,_>) yeilds a sequence->structure value`` (lang) =
    let inputDictionary = dict [| "firstDictEntryKey", Chair(); |]
    MtAssert.DestructuredAs(lang, "this {@will} work, I hope", [| inputDictionary |],
        expected = [
            PropertyNameAndValue("will",
                SequenceValue [
                    StructureValue("KeyValuePair`2",
                        [ PropertyNameAndValue("Key", ScalarValue "firstDictEntryKey")
                          PropertyNameAndValue("Value", chairStructureValue) ])
                ])
            ])

[<LangTheory; LangCsFsData>]
let ``a class instance is captured as a structure value`` (lang) =
    MtAssert.DestructuredAs(lang, "I sat at {@Chair}", [|Chair()|],
        [ PropertyNameAndValue("Chair", chairStructureValue) ])

[<LangTheory; LangCsFsData>]
let ``one positional property and one value yields the correct positional property`` (lang) =
    MtAssert.DestructuredAs(lang, "this {0} work, I hope", [|"will"|], [ PropertyNameAndValue("0", ScalarValue "will") ])

[<LangTheory; LangCsFsData>]
let ``multiple positional property and the same number of values yields the correct positional properties`` (lang) =
    MtAssert.DestructuredAs(lang, "{0} {1} {2} {3} {4} {5}, I hope", [|"this"; 10; true;"this"; 10; true|],
        [ PropertyNameAndValue("0", ScalarValue ("this"))
          PropertyNameAndValue("1", ScalarValue (10))
          PropertyNameAndValue("2", ScalarValue (true))
          PropertyNameAndValue("3", ScalarValue ("this"))
          PropertyNameAndValue("4", ScalarValue (10))
          PropertyNameAndValue("5", ScalarValue (true)) ])

type MyScalarEnum = Zero=0 | One=1 | Two=2

type ExpectedScalarResult = Different of obj | Same
    with override x.ToString() = sprintf "%A" x

let scalars : (obj * ExpectedScalarResult) list = [
    box (1s), Same
    box (2us), Same
    box (3), Same
    box (4u), Same
    box (5L), Same
    box (6UL), Same
    box ("7"), Same
    box (true), Same
    box (8uy), Same
    box (char 9), Same
    box (DateTime(2010, 10, 10)), Same
    box (DateTimeOffset(2000, 11, 11, 11, 11, 11, 11, offset=TimeSpan.FromHours 11.0)), Same
    box (12.12M), Same
    box (13.13), Same
    box (Guid.NewGuid()), Same
    box (null), Same
    box (14.14f), Same
    box (TimeSpan(15, 15, 15, 15)), Same
    box (Uri("http://localhost:1616/16")), Same
    box (box MyScalarEnum.Two), Same
    box (box MyScalarEnum.One), Same
    box (Nullable<int>(15)), Different (box 15)
    box (Nullable<int>()), Different (box null) 
]

let scalarInputAsObj (v:obj, e:ExpectedScalarResult) = 
    match e with Same -> v | Different de -> de
let getScalarExpected (v:obj, e:ExpectedScalarResult) =
    match e with Same -> v | Different de -> de

[<LangTheory; LangCsFsData>]
let ``scalar types are captured correctly when positional`` (lang) =
    let positionalFmtString = String.Join(" ", (scalars |> Seq.mapi (fun i _ -> "{" + string i + "}")))
    let valuesArray = scalars |> Seq.map (scalarInputAsObj) |> Seq.toArray
    let expected = scalars |> List.mapi (fun i s -> PropertyNameAndValue(string i, ScalarValue (getScalarExpected s)))
    MtAssert.DestructuredAs(lang, positionalFmtString, valuesArray, expected)

[<LangTheory; LangCsFsData>]
let ``scalar types are captured correctly when positionally out of order`` (lang) =
    let numberedOutOfOrder = scalars |> List.mapi (fun i items -> i, items) |> List.sortBy (fun x -> x.GetHashCode())
    let posTokensStringsOutOfOrder = numberedOutOfOrder |> Seq.map (fun (i, _) -> "{" + string i + "}")
    let outOfOrder = numberedOutOfOrder |> List.map (fun (i, items) -> items)
    let positionalFmtString = String.Join(" ", posTokensStringsOutOfOrder)
    let values = outOfOrder |> Seq.map (scalarInputAsObj) |> Seq.toArray
    let expected = outOfOrder |> List.mapi (fun i s -> PropertyNameAndValue(string i, ScalarValue (getScalarExpected s)))
    MtAssert.DestructuredAs(lang, positionalFmtString, values, expected)

[<LangTheory; LangCsFsData>]
let ``scalar types are captured correctly when named`` (lang) =
    let namedFmtString = String.Join(" ", (scalars |> Seq.mapi (fun i v -> "{named" + (string i) + "}")))
    let values = scalars |> Seq.map (fun s -> (scalarInputAsObj s)) |> Seq.toArray
    let expected = scalars |> List.mapi (fun i s -> PropertyNameAndValue("named" + string i, ScalarValue (getScalarExpected s)))
    MtAssert.DestructuredAs(lang, namedFmtString, values, expected)

[<LangTheory; LangCsFsData>]
let ``multiple positional property capture the correct integral scalar types`` (lang) =
    let values : obj[] = [| 1s; 2us; 3; 4u; 5L; 6UL |]
    MtAssert.DestructuredAs(lang, "{0} {1} {2}, I hope, {3} {4} {5}", values,
        [ PropertyNameAndValue("0", ScalarValue (box 1s))
          PropertyNameAndValue("1", ScalarValue (box 2us))
          PropertyNameAndValue("2", ScalarValue (box 3))
          PropertyNameAndValue("3", ScalarValue (box 4u))
          PropertyNameAndValue("4", ScalarValue (box 5L))
          PropertyNameAndValue("5", ScalarValue (box 6UL)) ])

[<LangTheory; LangCsFsData>]
let ``multiple positional nullable properties capture the correct integral scalar types`` (lang) =
    let values : obj[] = [|
        (Nullable 1s)
        (Nullable 2us)
        (Nullable 3)
        (Nullable 4u)
        (Nullable 5L)
        (Nullable 6UL)|]
    MtAssert.DestructuredAs(lang, "{0} {1} {2}, I hope, {3} {4} {5}", values,
        [ PropertyNameAndValue("0", ScalarValue (box 1s))
          PropertyNameAndValue("1", ScalarValue (box 2us))
          PropertyNameAndValue("2", ScalarValue (box 3))
          PropertyNameAndValue("3", ScalarValue (box 4u))
          PropertyNameAndValue("4", ScalarValue (box 5L))
          PropertyNameAndValue("5", ScalarValue (box 6UL)) ])

type MyDu = Case1 | Case2
type MyDuWithTuple =
| Case1AB of a:int * b:string
| Case2AB

[<AutoOpen>]
module St =
    /// named scalar value
    let scal (name, value) = PropertyNameAndValue(name, ScalarValue value)
    /// named structure value
    let strv (name, values) = StructureValue(name, values)
    /// property name and value
    let pnv (name, value) = PropertyNameAndValue(name, value)
    
[<LangTheory; LangCsFsData>]
let ``Default destructuring an F# union works`` (lang) =
    MtAssert.DestructuredAs(lang, "I like {@first} and {@second}", [| Case1; Case2 |],
        expected=[
            pnv("first",  strv ("MyDu", [ scal("Tag", 0); scal("IsCase1", true);  scal("IsCase2", false) ]))
            pnv("second", strv ("MyDu", [ scal("Tag", 1); scal("IsCase1", false); scal("IsCase2", true) ]))
        ])

    MtAssert.DestructuredAs(lang, "I like {@first} and {@second}", [| Case1AB(1,"2"); Case2AB |],
        expected=[
            pnv("first",  strv ("Case1AB", [ scal("a", 1)
                                             scal("b", "2")
                                             scal("Tag", 0)
                                             scal("IsCase1AB", true)
                                             scal("IsCase2AB", false) ]))
            pnv("second", strv (null, [ scal("Tag", 1)
                                        scal("IsCase1AB", false)
                                        scal("IsCase2AB", true) ]))
        ])

[<LangTheory; LangCsFsData>]
let ``Default destructuring an F# tuple works`` (lang) =
    
    let values : obj[] = [| 123,"abc"; 456, "def" |]
    let tyTag = values.[0].GetType().Name
    MtAssert.DestructuredAs(lang, "I like {@first} and {@second}", values,
        expected=[
            pnv("first",  strv (tyTag, [ scal("Item1", 123); scal("Item2", "abc"); ]))
            pnv("second", strv (tyTag, [ scal("Item1", 456); scal("Item2", "def"); ]))
        ])
    
    let values : obj[] = [| 1,2,3; 4,5,6 |]
    let tyTag = values.[0].GetType().Name
    MtAssert.DestructuredAs(lang, "I like {@first} and {@second}", values,
        expected=[
            pnv("first",  strv (tyTag, [ scal("Item1", 1); scal("Item2", 2); scal("Item3", 3); ]))
            pnv("second", strv (tyTag, [ scal("Item1", 4); scal("Item2", 5); scal("Item3", 6); ]))
        ])
