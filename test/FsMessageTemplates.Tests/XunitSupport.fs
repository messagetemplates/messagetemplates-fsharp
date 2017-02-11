[<AutoOpen>]
module FsTests.XunitSupport

open Xunit
open System
open System.Globalization

type LangTheoryAttribute() =
    inherit Xunit.TheoryAttribute()

type CSharpAndFSharpAttribute() =
    inherit Xunit.Sdk.DataAttribute()
    override __.GetData _ = [[|box "C#"|]; [|box "F#"|]] |> Seq.ofList

type FullParserImplementationsAttribute() =
    inherit Xunit.Sdk.DataAttribute()
    override __.GetData _ = [[|box "C#"|]; [|box "F#"|]; [|box "F#MtParserFull"|]] |> Seq.ofList

/// Indicates that all implementations must pass the test.
type AllImplementationsAttribute() =
    inherit Xunit.Sdk.DataAttribute()
    override __.GetData _ = [[|box "C#"|]; [|box "F#"|]; [|box "F#MtParser"|]; [|box "F#MtParserFull"|]] |> Seq.ofList
