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
