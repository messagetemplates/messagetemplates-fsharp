module FsTests.Asserts

open System
open FsMessageTemplates
open System.Collections.Generic

let invariantProvider = (System.Globalization.CultureInfo.InvariantCulture :> System.IFormatProvider)

type FsToken = FsMessageTemplates.Token
type FsDestr = FsMessageTemplates.DestrHint
type FsAlign = FsMessageTemplates.AlignInfo
type FsProp = FsMessageTemplates.Property
type FsMtParserToken = TextToken of string | PropToken of FsMtParser.Property
type FsMtParserFullToken = TextToken of string | PropToken of FsMtParserFull.Property

let captureHintToDestrHit = function
                            | FsMtParserFull.CaptureHint.Default -> DestrHint.Default
                            | FsMtParserFull.CaptureHint.Stringify -> DestrHint.Stringify
                            | FsMtParserFull.CaptureHint.Structure -> DestrHint.Destructure
                            | ch -> failwithf "unexpected CatureHint %A" ch

let toAlignInfo (ai : FsMtParserFull.AlignInfo) =
    if ai.isEmpty then AlignInfo.Empty
    else
        let direction = match ai.direction with FsMtParserFull.AlignDirection.Left -> Direction.Left | _ -> Direction.Right
        AlignInfo(direction, ai.width)

/// Parses message templates from the different implementations in semi-compatible ways by 
/// using FsMessageTemplates.Token as the target type.
let parsedAs lang message (expectedTokens: FsToken seq) =
    let mutable ignoreStartIndex = false
    let parsed =
        match lang with
        | "C#" -> MessageTemplates.MessageTemplate.Parse(message).Tokens |> Seq.map CsToFs.mttToToken |> List.ofSeq
        | "F#" -> (FsMessageTemplates.Parser.parse message).Tokens |> List.ofSeq
        | "F#MtParser" ->
          ignoreStartIndex <- true // FsMtParser doesn't support index
          let tokens = ResizeArray<FsMtParserToken>()
          let foundText s = tokens.Add(FsMtParserToken.TextToken(s))
          let foundProp p = tokens.Add(FsMtParserToken.PropToken(p))
          FsMtParser.parseParts message foundText foundProp
          tokens
          |> Seq.map (function
            | FsMtParserToken.TextToken s -> FsToken.TextToken(0, s)
            | FsMtParserToken.PropToken p -> FsToken.PropToken(0, FsProp(p.name, -1, FsDestr.Default, FsAlign.Empty, p.format)) )
          |> List.ofSeq
        | "F#MtParserFull" ->
          ignoreStartIndex <- true // FsMtParser doesn't support index
          let tokens = ResizeArray<FsMtParserFullToken>()
          let foundText s = tokens.Add(FsMtParserFullToken.TextToken(s))
          let foundProp p = tokens.Add(FsMtParserFullToken.PropToken(p))
          FsMtParserFull.parseParts message foundText foundProp
          tokens
          |> Seq.map (function
            | FsMtParserFullToken.TextToken s -> FsToken.TextToken(0, s)
            | FsMtParserFullToken.PropToken p -> FsToken.PropToken(0, FsProp(p.name, -1, captureHintToDestrHit p.captureHint, toAlignInfo p.align, p.format)) )
          |> List.ofSeq
        | other -> failwithf "unexpected lang '%s'" other

    let setStartIndexZeroIfIgnored tokens =
      match ignoreStartIndex with
      | false -> tokens
      | true -> tokens |> Seq.map (function
                                    | FsToken.TextToken (i, t) -> FsToken.TextToken(0, t)
                                    | FsToken.PropToken (i, p) -> FsToken.PropToken(0, p))

    let expected = expectedTokens |> Seq.cast<FsToken> |> setStartIndexZeroIfIgnored |> Seq.toList
    Xunit.Assert.Equal<FsToken list> (expected, parsed)

let capture lang (messageTemplate:string) (args: obj list) =
    let argsArray = (args |> Seq.cast<obj> |> Seq.toArray) // force 'args' to be IEnumerable
    match lang with
    | "F#" -> FsMessageTemplates.Capturing.captureMessageProperties messageTemplate argsArray |> List.ofSeq
    | "C#" -> MessageTemplates.MessageTemplate.Capture(messageTemplate, argsArray) |> Seq.map CsToFs.templateProperty |> List.ofSeq
    | other -> failwithf "unexpected lang '%s'" other

let renderp lang (provider:IFormatProvider) messageTemplate args =
    let argsArray = (args |> Seq.cast<obj> |> Seq.toArray) // force 'args' to be IEnumerable
    match lang with
    | "C#" -> MessageTemplates.MessageTemplate.Format(provider, messageTemplate, argsArray)
    | "F#" -> FsMessageTemplates.Formatting.sprintsm provider messageTemplate argsArray
    | other -> failwithf "unexpected lang '%s'" other

let render lang template args =
    renderp lang System.Globalization.CultureInfo.InvariantCulture template args

type MtAssert() =

    /// Asserts the 
    static member ParsedAs (lang, message,  expectedTokens: FsToken seq) =
        parsedAs lang message expectedTokens

    /// Captures properties from the C# or F# version in compatible ways.
    static member internal Capture(lang, template, values, ?maxDepth, ?additionalScalars, ?additionalDestrs) =
        let maxDepth = defaultArg maxDepth 10
        let additionalScalars = defaultArg additionalScalars Seq.empty<Type>
        let additionalDestrs = defaultArg additionalDestrs Seq.empty<FsMessageTemplates.Destructurer>
        match lang with
        | "C#" ->
            let mt = MessageTemplates.Parsing.MessageTemplateParser().Parse template
            let csDestrs = additionalDestrs |> Seq.map CsToFs.fsDestrToCsDestrPolicy
            let captured = CsToFs.CsMt.CaptureWith(maxDepth, additionalScalars, csDestrs, mt, values)
            captured |> Seq.map CsToFs.templateProperty |> Seq.toList
        | "F#" ->
            let mt = FsMessageTemplates.Parser.parse template
            let emptyKeepTrying = Unchecked.defaultof<TemplatePropertyValue>
            let tryScalars : Destructurer = fun r ->
                let exists = additionalScalars |> Seq.exists ((=) (r.Value.GetType()))
                if exists then ScalarValue(r.Value) else emptyKeepTrying
            let destrNoneForNull (r: DestructureRequest) (d:Destructurer) =
                match d (DestructureRequest (r.Destructurer, r.Value, 10, 1, hint=r.Hint)) with
                | tpv when tpv = TemplatePropertyValue.Empty -> None
                | tpv -> Some tpv
            let tryDestrs : Destructurer = fun r ->
                match additionalDestrs |> Seq.tryPick (destrNoneForNull r) with
                | None -> emptyKeepTrying
                | Some tpv -> tpv
            let destr = Capturing.createCustomDestructurer (Some tryScalars) (Some tryDestrs)
            FsMessageTemplates.Capturing.capturePropertiesCustom destr maxDepth mt values
            |> Seq.toList
        | other -> failwithf "unexpected lang %s" other

    /// Formats either the C# or F# version in compatible ways.
    static member internal Format(lang, template, values, ?provider, ?maxDepth, ?additionalScalars, ?additionalDestrs) =
        let provider = defaultArg provider invariantProvider
        let maxDepth = defaultArg maxDepth 10
        let additionalScalars = defaultArg additionalScalars Seq.empty<Type>
        let additionalDestrs = defaultArg additionalDestrs Seq.empty<FsMessageTemplates.Destructurer>
        match lang with
        | "C#" ->
            let mt = MessageTemplates.Parsing.MessageTemplateParser().Parse template
            let csDestrs = additionalDestrs |> Seq.map CsToFs.fsDestrToCsDestrPolicy
            let captured = CsToFs.CsMt.CaptureWith(maxDepth, additionalScalars, csDestrs, mt, values)
            let propsByName = captured
                              |> fun tpvl -> MessageTemplates.Core.TemplatePropertyValueDictionary(tpvl)

            mt.Render(properties=propsByName, formatProvider=provider)
        | "F#" ->
            let mt = FsMessageTemplates.Parser.parse template
            let emptyKeepTrying = Unchecked.defaultof<TemplatePropertyValue>
            let tryScalars : Destructurer = fun r ->
                let exists = additionalScalars |> Seq.exists ((=) (r.Value.GetType()))
                if exists then ScalarValue(r.Value) else emptyKeepTrying
            let destrNoneForNull (r: DestructureRequest) (d:Destructurer) =
                match d (DestructureRequest (r.Destructurer, r.Value, 10, 1, hint=r.Hint)) with
                | tpv when tpv = TemplatePropertyValue.Empty -> None
                | tpv -> Some tpv
            let tryDestrs : Destructurer = fun r ->
                match additionalDestrs |> Seq.tryPick (destrNoneForNull r) with
                | None -> emptyKeepTrying
                | Some tpv -> tpv
            let destr = Capturing.createCustomDestructurer (Some tryScalars) (Some tryDestrs)
            let propsByName = FsMessageTemplates.Capturing.capturePropertiesCustom destr maxDepth mt values
                              |> Seq.map (fun tpv -> tpv.Name, tpv.Value)
                              |> dict
            let getValueByName name =
                let exists, value = propsByName.TryGetValue(name)
                if exists then value else TemplatePropertyValue.Empty
            use tw = new System.IO.StringWriter(formatProvider=provider)
            FsMessageTemplates.Formatting.formatCustom mt tw getValueByName
            let formatCustomOutput = tw.ToString()

            // if invariant and no custom scalars and types provided, verify 'format' gives the same result
            // if maxDepth is different, we also can't assert the other overloads produce the same output, as
            // only formatCustom allows this to change.
            if maxDepth=10 && additionalScalars = Seq.empty<Type> && additionalDestrs = Seq.empty<Destructurer> then
                if provider = invariantProvider then
                    // format should be the same
                    let formatOutput = FsMessageTemplates.Formatting.format mt values
                    Xunit.Assert.Equal (formatCustomOutput, formatOutput)

                    // bprintm should be the same
                    let sb = System.Text.StringBuilder()
                    FsMessageTemplates.Formatting.bprintm mt sb values
                    Xunit.Assert.Equal (formatCustomOutput, sb.ToString())

                    // bprintsm should be the same
                    sb.Clear() |> ignore
                    FsMessageTemplates.Formatting.bprintsm sb template values
                    Xunit.Assert.Equal (formatCustomOutput, sb.ToString())

                // fprintm should be the same
                use fprintTw = new System.IO.StringWriter(formatProvider=provider)
                FsMessageTemplates.Formatting.fprintm mt fprintTw values
                Xunit.Assert.Equal (formatCustomOutput, fprintTw.ToString())

                // fprintsm should be the same
                use fprintsTw = new System.IO.StringWriter(formatProvider=provider)
                FsMessageTemplates.Formatting.fprintsm fprintsTw template values
                Xunit.Assert.Equal (formatCustomOutput, fprintsTw.ToString())

                // sprint*m should be the same
                Xunit.Assert.Equal (formatCustomOutput, FsMessageTemplates.Formatting.sprintm mt provider values)
                Xunit.Assert.Equal (formatCustomOutput, FsMessageTemplates.Formatting.sprintsm provider template values)

            formatCustomOutput

        | other -> failwithf "unexpected lang %s" other

    static member DestructuredAs(lang, template, values, expected, ?maxDepth, ?additionalScalars, ?additionalDestrs) =
        let maxDepth = defaultArg maxDepth 10
        let additionalScalars = defaultArg additionalScalars Seq.empty<Type>
        let additionalDestrs = defaultArg additionalDestrs Seq.empty<FsMessageTemplates.Destructurer>
        let actual = MtAssert.Capture(lang, template, values, maxDepth, additionalScalars, additionalDestrs)

        Xunit.Assert.StrictEqual (expected, actual)

    static member RenderedAs(lang, template, values, expected, ?provider, ?maxDepth, ?additionalScalars, ?additionalDestrs) =
        let provider = defaultArg provider invariantProvider
        let maxDepth = defaultArg maxDepth 10
        let additionalScalars = defaultArg additionalScalars Seq.empty<Type>
        let additionalDestrs = defaultArg additionalDestrs Seq.empty<FsMessageTemplates.Destructurer>
        let actual = MtAssert.Format(lang, template, values, provider, maxDepth, additionalScalars, additionalDestrs)

        // Using XUnit.Assert because it has a better message on failure for string compares
        Xunit.Assert.Equal (expected, actual)
