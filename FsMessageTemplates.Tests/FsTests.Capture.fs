module FsTests.Capture

open System
open Swensen.Unquote

open Tk
open FsMessageTemplates

[<LangTheory; LangCsFsData>]
let ``no values provided yields no properties`` (lang) =
    let actual = capture lang "this {will} {capture} {nothing}, I hope" []
    let expected : PropertyNameAndValue list = []
    test <@ actual = expected @>

[<LangTheory; LangCsFsData>]
let ``one named property and one value yields the correct named property`` (lang) =
    let actual = capture lang "this {will} work, I hope" [box "might"]
    test <@ actual = [ PropertyNameAndValue("will", ScalarValue "might") ] @>

[<LangTheory; LangCsFsData>]
let ``one enumerable property yeilds a sequence value`` (lang) =
    let actual = capture lang "this {will} work, I hope" [box [|1..3|]]
    let expectedValue = SequenceValue [ ScalarValue 1; ScalarValue 2; ScalarValue 3; ]
    let expected =  [ PropertyNameAndValue("will", expectedValue) ]
    test <@ actual = expected @>

type Chair() =
    member __.Back with get() = "straight"
    member __.Legs with get() = [|1;2;3;4|]
    override __.ToString() = "a chair"

let chairStructureValue =
    let propNamesAndValues = [| PropertyNameAndValue("Back", ScalarValue "straight")
                                PropertyNameAndValue("Legs", SequenceValue ([ ScalarValue 1; ScalarValue 2; ScalarValue 3; ScalarValue 4 ]))
                             |]
    StructureValue ("Chair", propNamesAndValues)

[<LangTheory; LangCsFsData>]
let ``a destructured dictionary yeilds dictionary values`` (lang) =
    let inputDictionary = System.Collections.Generic.Dictionary(dict [| "key", Chair(); |])
    let actual = capture lang "this {@will} work, I hope" [ box inputDictionary ]
    let expected = [ PropertyNameAndValue("will", DictionaryValue [ ScalarValue "key", chairStructureValue ]) ]
    test <@ actual = expected @>

[<LangTheory; LangCsFsData>]
let ``an F# 'dict' (which is not Dictionary<_,_>) yeilds a sequence->structure value`` (lang) =
    let inputDictionary = dict [| "firstDictEntryKey", Chair(); |]
    let actual = capture lang "this {@will} work, I hope" [ box inputDictionary ]
    let expected = [ PropertyNameAndValue("will", SequenceValue [StructureValue("KeyValuePair`2", [| PropertyNameAndValue("Key", ScalarValue "firstDictEntryKey")
                                                                                                     PropertyNameAndValue("Value", chairStructureValue)
                                                                                                  |])
                                                                ])
                   ]
    test <@ actual = expected @>

[<LangTheory; LangCsFsData>]
let ``a class instance is captured as a structure value`` (lang) =
    let actual = capture lang "I sat at {@Chair}" [Chair()]
    let expected : PropertyNameAndValue list = [ PropertyNameAndValue("Chair", chairStructureValue) ]
    test <@ actual = expected @>

[<LangTheory; LangCsFsData>]
let ``one positional property and one value yields the correct positional property`` (lang) =
    let actual = capture lang "this {0} work, I hope" [box "will"]
    test <@ actual = [ PropertyNameAndValue("0", ScalarValue "will") ] @>

[<LangTheory; LangCsFsData>]
let ``multiple positional property and the same number of values yields the correct positional properties`` (lang) =
    let actual = capture lang "{0} {1} {2}, I hope" [box "this"; box 10; box true]
    test <@ actual = [ PropertyNameAndValue("0", ScalarValue ("this"))
                       PropertyNameAndValue("1", ScalarValue (10))
                       PropertyNameAndValue("2", ScalarValue (true)) ] @>

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
    let actual = capture lang positionalFmtString valuesArray
    test <@ actual = expected @>

[<LangTheory; LangCsFsData>]
let ``scalar types are captured correctly when positionally out of order`` (lang) =
    let numberedOutOfOrder = scalars |> List.mapi (fun i items -> i, items) |> List.sortBy (fun x -> x.GetHashCode())
    let posTokensStringsOutOfOrder = numberedOutOfOrder |> Seq.map (fun (i, _) -> "{" + string i + "}")
    let outOfOrder = numberedOutOfOrder |> List.map (fun (i, items) -> items)
    let positionalFmtString = String.Join(" ", posTokensStringsOutOfOrder)
    let valuesArray = outOfOrder |> Seq.map (scalarInputAsObj) |> Seq.toArray
    let expected = outOfOrder |> List.mapi (fun i s -> PropertyNameAndValue(string i, ScalarValue (getScalarExpected s)))
    let actual = capture lang positionalFmtString valuesArray
    test <@ actual = expected @>

[<LangTheory; LangCsFsData>]
let ``scalar types are captured correctly when named`` (lang) =
    let namedFmtString = String.Join(" ", (scalars |> Seq.mapi (fun i v -> "{named" + (string i) + "}")))
    let valuesArray = scalars |> Seq.map (fun s -> (scalarInputAsObj s)) |> Seq.toArray
    let expected = scalars |> List.mapi (fun i s -> PropertyNameAndValue("named" + string i, ScalarValue (getScalarExpected s)))
    let actual = capture lang namedFmtString valuesArray
    test <@ actual = expected @>

[<LangTheory; LangCsFsData>]
let ``multiple positional property capture the correct integral scalar types`` (lang) =
    let values = [  box     1s
                    box     2us
                    box     3
                    box     4u
                    box     5L
                    box     6UL ]
    let actual = capture lang "{0} {1} {2}, I hope, {3} {4} {5}" values
    test <@ actual = [ PropertyNameAndValue("0", ScalarValue (box 1s))
                       PropertyNameAndValue("1", ScalarValue (box 2us))
                       PropertyNameAndValue("2", ScalarValue (box 3))
                       PropertyNameAndValue("3", ScalarValue (box 4u))
                       PropertyNameAndValue("4", ScalarValue (box 5L))
                       PropertyNameAndValue("5", ScalarValue (box 6UL)) ] @>

[<LangTheory; LangCsFsData>]
let ``multiple positional nullable properties capture the correct integral scalar types`` (lang) =
    let values = [  box     (Nullable 1s)
                    box     (Nullable 2us)
                    box     (Nullable 3)
                    box     (Nullable 4u)
                    box     (Nullable 5L)
                    box     (Nullable 6UL) ]
    let actual = capture lang "{0} {1} {2}, I hope, {3} {4} {5}" values
    test <@ actual = [ PropertyNameAndValue("0", ScalarValue (box 1s))
                       PropertyNameAndValue("1", ScalarValue (box 2us))
                       PropertyNameAndValue("2", ScalarValue (box 3))
                       PropertyNameAndValue("3", ScalarValue (box 4u))
                       PropertyNameAndValue("4", ScalarValue (box 5L))
                       PropertyNameAndValue("5", ScalarValue (box 6UL)) ] @>


