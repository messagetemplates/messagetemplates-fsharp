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
    let actual = capture lang "{0} {1} {2}, I hope" [box "this"; box "will"; box "work"]
    test <@ actual = [ "0", ScalarValue (Scalar.String "this")
                       "1", ScalarValue (Scalar.String "will")
                       "2", ScalarValue (Scalar.String "work") ] @>

[<LangTheory; LangCsFsData>]
let ``multiple positional property capture the correct integral scalar types`` (lang) =
    let values = [  box<int16>      1s
                    box<uint16>     2us
                    box<int32>      3
                    box<uint32>     4u
                    box<int64>      5L
                    box<uint64>     6UL ]
    let actual = capture lang "{0} {1} {2}, I hope, {3} {4} {5}" values
    test <@ actual = [ "0", ScalarValue (Scalar.Int16 1s)
                       "1", ScalarValue (Scalar.UInt16 2us)
                       "2", ScalarValue (Scalar.Int32 3)
                       "3", ScalarValue (Scalar.UInt32 4u)
                       "4", ScalarValue (Scalar.Int64 5L)
                       "5", ScalarValue (Scalar.UInt64 6UL) ] @>


