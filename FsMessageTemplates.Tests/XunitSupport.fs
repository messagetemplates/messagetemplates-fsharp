[<AutoOpen>]
module FsTests.XunitSupport

open Xunit
open System
open System.Globalization
open Swensen.Unquote

type LangTheoryAttribute() =
    inherit Xunit.TheoryAttribute()
type LangCsFsDataAttribute() =
    inherit Xunit.Sdk.DataAttribute()
    override __.GetData _ = [[|box "C#"|]; [|box "F#"|]] |> Seq.ofList

type FsToken = FsMessageTemplates.MessageTemplates.Token

let assertParsedAs lang message (expectedTokens: System.Collections.IEnumerable) =
    let parsed =
        match lang with
        | "C#" -> MessageTemplates.MessageTemplate.Parse(message).Tokens |> Seq.map CsToFs.mttToToken |> List.ofSeq
        | "F#" -> (FsMessageTemplates.MessageTemplates.parse message).Tokens
        | other -> failwithf "unexpected lang '%s'" other

    let expected = expectedTokens |> Seq.cast<FsToken> |> Seq.toList
    test <@ parsed = expected @>

let capture lang (messageTemplate:string) args =
    let argsArray = (args |> Seq.cast<obj> |> Seq.toArray) // force 'args' to be IEnumerable
    match lang with
    | "F#" -> FsMessageTemplates.MessageTemplates.captureMessageProperties messageTemplate argsArray
    | "C#" -> MessageTemplates.MessageTemplate.Capture(messageTemplate, argsArray) |> Seq.map CsToFs.templateProperty |> List.ofSeq
    | other -> failwithf "unexpected lang '%s'" other

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
