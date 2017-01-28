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

/// Just like CSharpAndFSharpAttribute, only includes a test for the FsMtParser.
type AllImplementationsAttribute() =
    inherit Xunit.Sdk.DataAttribute()
    override __.GetData _ = [[|box "C#"|]; [|box "F#"|]; [|box "F#MtParser"|] ] |> Seq.ofList
