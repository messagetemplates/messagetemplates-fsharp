module FsTests.Asserts

open System
open FsMessageTemplates
open Swensen.Unquote
open System.Collections.Generic

type MtAssert() =
    /// Captures properties from the C# or F# version in compatible ways.
    static member internal Capture(lang, template, values, ?depth, ?additionalScalars, ?additionalDestrs) =
        let depth = defaultArg depth 10
        let additionalScalars = defaultArg additionalScalars Seq.empty<Type>
        let additionalDestrs = defaultArg additionalDestrs Seq.empty<FsMessageTemplates.Destructurer>
        match lang with
        | "C#" ->
            let mt = MessageTemplates.Parsing.MessageTemplateParser().Parse template
            let csDestrs = additionalDestrs |> Seq.map CsToFs.fsDestrToCsDestrPolicy
            let captured = CsToFs.CsMt.CaptureWith(depth, additionalScalars, csDestrs, mt, values)
            captured |> Seq.map CsToFs.templateProperty |> Seq.toList                
        | "F#" ->
            let mt = FsMessageTemplates.Parser.parse template
            let emptyKeepTrying = Unchecked.defaultof<TemplatePropertyValue>
            let tryScalars : Destructurer = fun r ->
                let exists = additionalScalars |> Seq.exists ((=) (r.Value.GetType()))
                if exists then ScalarValue(r.Value) else emptyKeepTrying
            let destrNoneForNull (r: DestructureRequest) (d:Destructurer) =
                match d (DestructureRequest (r.Hint, r.Value, r.Destr)) with
                | tpv when Capturing.isEmptyKeepTrying tpv -> None
                | tpv -> Some tpv
            let tryDestrs : Destructurer = fun r ->
                match additionalDestrs |> Seq.tryPick (destrNoneForNull r) with
                | None -> emptyKeepTrying
                | Some tpv -> tpv
            let destr = Capturing.createCustomDestructurer (Some tryScalars) (Some tryDestrs)
            FsMessageTemplates.Capturing.capturePropertiesCustom (destr) mt values
            |> Seq.toList
        | other -> failwithf "unexpected lang %s" other

    /// Formats either the C# or F# version in compatible ways.
    static member internal Format(lang, template, values, ?provider, ?depth, ?additionalScalars, ?additionalDestrs) =
        let provider = defaultArg provider System.Globalization.CultureInfo.InvariantCulture
        let depth = defaultArg depth 10
        let additionalScalars = defaultArg additionalScalars Seq.empty<Type>
        let additionalDestrs = defaultArg additionalDestrs Seq.empty<FsMessageTemplates.Destructurer>
        match lang with
        | "C#" ->
            let mt = MessageTemplates.Parsing.MessageTemplateParser().Parse template
            let csDestrs = additionalDestrs |> Seq.map CsToFs.fsDestrToCsDestrPolicy
            let captured = CsToFs.CsMt.CaptureWith(depth, additionalScalars, csDestrs, mt, values)
            let propsByName = captured |> Seq.map (fun tp -> tp.Name, tp.Value) |> dict |> Dictionary<string, CsToFs.CsTemplatePropertyValue>
            mt.Render(properties=propsByName, formatProvider=provider)
        | "F#" ->
            let mt = FsMessageTemplates.Parser.parse template
            let emptyKeepTrying = Unchecked.defaultof<TemplatePropertyValue>
            let tryScalars : Destructurer = fun r ->
                let exists = additionalScalars |> Seq.exists ((=) (r.Value.GetType()))
                if exists then ScalarValue(r.Value) else emptyKeepTrying
            let destrNoneForNull (r: DestructureRequest) (d:Destructurer) =
                match d (DestructureRequest (r.Hint, r.Value, r.Destr)) with
                | tpv when Capturing.isEmptyKeepTrying tpv -> None
                | tpv -> Some tpv
            let tryDestrs : Destructurer = fun r ->
                match additionalDestrs |> Seq.tryPick (destrNoneForNull r) with
                | None -> emptyKeepTrying
                | Some tpv -> tpv
            let destr = Capturing.createCustomDestructurer (Some tryScalars) (Some tryDestrs)
            let propsByName = FsMessageTemplates.Capturing.capturePropertiesCustom (destr) mt values
                              |> Seq.map (fun tpv -> tpv.Name, tpv.Value)
                              |> dict
            let getValueByName name =
                let exists, value = propsByName.TryGetValue(name)
                if exists then value else Unchecked.defaultof<TemplatePropertyValue>
            use tw = new System.IO.StringWriter(formatProvider=provider)
            FsMessageTemplates.Formatting.formatCustom mt tw getValueByName
            tw.ToString()
        | other -> failwithf "unexpected lang %s" other

    static member DestructuredAs(lang, template, values, expected, ?depth, ?additionalScalars, ?additionalDestrs) =
        let depth = defaultArg depth 10
        let additionalScalars = defaultArg additionalScalars Seq.empty<Type>
        let additionalDestrs = defaultArg additionalDestrs Seq.empty<FsMessageTemplates.Destructurer>
        let actual = MtAssert.Capture(lang, template, values, depth, additionalScalars, additionalDestrs)
        test <@ actual = expected @>

    static member RenderedAs(lang, template, values, expected, ?provider, ?depth, ?additionalScalars, ?additionalDestrs) =
        let provider = defaultArg provider System.Globalization.CultureInfo.InvariantCulture
        let depth = defaultArg depth 10
        let additionalScalars = defaultArg additionalScalars Seq.empty<Type>
        let additionalDestrs = defaultArg additionalDestrs Seq.empty<FsMessageTemplates.Destructurer>
        let actual = MtAssert.Format(lang, template, values, provider, depth, additionalScalars, additionalDestrs)

        // Using XUnit.Assert because it has a better message on failure
        Xunit.Assert.Equal (expected, actual)
