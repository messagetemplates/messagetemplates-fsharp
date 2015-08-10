module FsTests.Capture

open System
open System.Globalization
open Swensen.Unquote

open FsMessageTemplates.MessageTemplates

[<LangTheory; LangCsFsData>]
let ``no values provided yields no properties`` (lang) =
    let actual = capture lang "this {will} {capture} {nothing}, I hope" []
    let expected : PropertyNameAndValue list = []
    test <@ actual = expected @>

[<LangTheory; LangCsFsData>]
let ``one named property and one value yields the correct named property`` (lang) =
    let actual = capture lang "this {will} work, I hope" [box "might"]
    test <@ actual = [ "will", ScalarValue (Scalar.String "might") ] @>

[<LangTheory; LangCsFsData>]
let ``one positional property and one value yields the correct positional property`` (lang) =
    let actual = capture lang "this {0} work, I hope" [box "will"]
    test <@ actual = [ "0", ScalarValue (Scalar.String "will") ] @>

[<LangTheory; LangCsFsData>]
let ``multiple positional property and the same number of values yields the correct positional properties`` (lang) =
    let actual = capture lang "{0} {1} {2}, I hope" [box "this"; box 10; box true]
    test <@ actual = [ "0", ScalarValue (Scalar.String "this")
                       "1", ScalarValue (Scalar.Int32 10)
                       "2", ScalarValue (Scalar.Bool true) ] @>

type MyScalarEnum = Zero=0 | One=1 | Two=2

type ExpectedScalarResult = Different of Scalar | Same
    with override x.ToString() = sprintf "%A" x

let scalars = [
    (Scalar.Int16 1s), Same
    (Scalar.UInt16 2us), Same
    (Scalar.Int32 3), Same
    (Scalar.UInt32 4u), Same
    (Scalar.Int64 5L), Same
    (Scalar.UInt64 6UL), Same
    (Scalar.String "7"), Same
    (Scalar.Bool true), Same
    (Scalar.Byte 8uy), Same
    (Scalar.Char (char 9)), Same
    (Scalar.DateTime (DateTime(2010, 10, 10))), Same
    (Scalar.DateTimeOffset (DateTimeOffset(2000, 11, 11, 11, 11, 11, 11, offset=(TimeSpan.FromHours 11.0)))), Same
    (Scalar.Decimal 12.12M), Same
    (Scalar.Double 13.13), Same
    (Scalar.Guid (Guid.NewGuid())), Same
    (Scalar.Null), Same
    (Scalar.Single 14.14f), Same
    (Scalar.TimeSpan (TimeSpan(15, 15, 15, 15))), Same
    (Scalar.Uri (Uri("http://localhost:1616/16"))), Same
    (Scalar.Other (box MyScalarEnum.Two)), Same
    (Scalar.Other (box MyScalarEnum.One)), Same
    (Scalar.Other (Nullable<int>(15))), Different (Scalar.Int32 15)
    (Scalar.Other (Nullable<int>())), Different (Scalar.Null) 
]
let scalarInputAsObj (v:Scalar, e:ExpectedScalarResult) = 
    match e with Same -> v.GetValueAsObject() | Different de -> de.GetValueAsObject()
let getScalarExpected (v:Scalar, e:ExpectedScalarResult) = match e with Same -> v | Different de -> de

[<LangTheory; LangCsFsData>]
let ``scalar types are captured correctly when positional`` (lang) =
    let positionalFmtString = String.Join(" ", (scalars |> Seq.mapi (fun i _ -> "{" + string i + "}")))
    let valuesArray = scalars |> Seq.map (scalarInputAsObj) |> Seq.toArray
    let expected = scalars |> List.mapi (fun i s -> string i, ScalarValue (getScalarExpected s))
    let actual = capture lang positionalFmtString valuesArray
    test <@ actual = expected @>

[<LangTheory; LangCsFsData>]
let ``scalar types are captured correctly when positionally out of order`` (lang) =
    let numberedOutOfOrder = scalars |> List.mapi (fun i items -> i, items) |> List.sortBy (fun x -> x.GetHashCode())
    let posTokensStringsOutOfOrder = numberedOutOfOrder |> Seq.map (fun (i, _) -> "{" + string i + "}")
    let outOfOrder = numberedOutOfOrder |> List.map (fun (i, items) -> items)
    let positionalFmtString = String.Join(" ", posTokensStringsOutOfOrder)
    let valuesArray = outOfOrder |> Seq.map (scalarInputAsObj) |> Seq.toArray
    let expected = outOfOrder |> List.mapi (fun i s -> string i, ScalarValue (getScalarExpected s))
    let actual = capture lang positionalFmtString valuesArray
    test <@ actual = expected @>

[<LangTheory; LangCsFsData>]
let ``scalar types are captured correctly when named`` (lang) =
    let namedFmtString = String.Join(" ", (scalars |> Seq.mapi (fun i v -> "{named" + (string i) + "}")))
    let valuesArray = scalars |> Seq.map (fun s -> (scalarInputAsObj s)) |> Seq.toArray
    let expected = scalars |> List.mapi (fun i s -> "named" + string i, ScalarValue (getScalarExpected s))
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
    test <@ actual = [ "0", ScalarValue (Scalar.Int16 1s)
                       "1", ScalarValue (Scalar.UInt16 2us)
                       "2", ScalarValue (Scalar.Int32 3)
                       "3", ScalarValue (Scalar.UInt32 4u)
                       "4", ScalarValue (Scalar.Int64 5L)
                       "5", ScalarValue (Scalar.UInt64 6UL) ] @>

[<LangTheory; LangCsFsData>]
let ``multiple positional nullable properties capture the correct integral scalar types`` (lang) =
    let values = [  box     (Nullable 1s)
                    box     (Nullable 2us)
                    box     (Nullable 3)
                    box     (Nullable 4u)
                    box     (Nullable 5L)
                    box     (Nullable 6UL) ]
    let actual = capture lang "{0} {1} {2}, I hope, {3} {4} {5}" values
    test <@ actual = [ "0", ScalarValue (Scalar.Int16 1s)
                       "1", ScalarValue (Scalar.UInt16 2us)
                       "2", ScalarValue (Scalar.Int32 3)
                       "3", ScalarValue (Scalar.UInt32 4u)
                       "4", ScalarValue (Scalar.Int64 5L)
                       "5", ScalarValue (Scalar.UInt64 6UL) ] @>


