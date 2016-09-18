[<AutoOpen>]
module FsTests.XunitSupport

open Xunit
open System
open System.Globalization

type LangTheoryAttribute() =
    inherit Xunit.TheoryAttribute()
type LangCsFsDataAttribute() =
    inherit Xunit.Sdk.DataAttribute()
    override __.GetData _ = [[|box "C#"|]; [|box "F#"|]] |> Seq.ofList

/// Just like LangCsFsDataAttribute, only includes a test for the FsMtParser.
type LangCsFsAndFsMtParserDataAttribute() =
    inherit Xunit.Sdk.DataAttribute()
    override __.GetData _ = [[|box "C#"|]; [|box "F#"|]; [|box "F#MtParser"|] ] |> Seq.ofList
