module FsTests.Asserts

open System
open System.Collections.Generic

let invariantProvider = (System.Globalization.CultureInfo.InvariantCulture :> System.IFormatProvider)

/// Attempts to make things less confusing by using aliases for
/// the different implementations. Not sure this is working at all.
module Impl =
  module Cs =
    let parse template = MessageTemplates.MessageTemplate.Parse template
    type TemplatePropertyValueDictionary = MessageTemplates.Core.TemplatePropertyValueDictionary
  module Fs =
    let parse template = FsMessageTemplates.Parser.parse template
    type Token = FsMessageTemplates.Token
    type Destr = FsMessageTemplates.DestrHint
    type Align = FsMessageTemplates.AlignInfo
    type AlignDir = FsMessageTemplates.Direction
    type Prop = FsMessageTemplates.Property
    type DestrRequest = FsMessageTemplates.DestructureRequest
    type Destructurer = FsMessageTemplates.Destructurer
    type TemplatePropertyValue = FsMessageTemplates.TemplatePropertyValue
    let ScalarValue v = FsMessageTemplates.ScalarValue v
  module FsMt =
    type Prop = FsMtParser.Property
    type ParserToken = TextToken of string | PropToken of FsMtParser.Property
    type Destr = FsMtParser.CaptureHint
    type Align = FsMtParser.AlignInfo
    type AlignDir = FsMtParser.AlignDirection

/// Use this common model because it's easier to print and compare
module TestModel =
  type AlignDirection = Right | Left
  type AlignInfo = { Direction: AlignDirection; Width: int }
  type CaptureHint = Default | Stringify | Structure
  type Property = {
    Name: string
    IsPositional: bool
    CaptureHint: CaptureHint
    Format: string option
    Align: AlignInfo option
  } with
    override x.ToString() = sprintf "%+A" x

  type Token =
    | TextToken of startIndex:int * text:string
    | PropToken of startIndex:int * prop:Property
    with override x.ToString() = sprintf "%+A" x

  let fromFsHint = function
    | Impl.Fs.Destr.Default -> CaptureHint.Default
    | Impl.Fs.Destr.Stringify -> CaptureHint.Stringify
    | Impl.Fs.Destr.Destructure -> CaptureHint.Structure
    | destr -> failwithf "unexpected %A" destr

  let fromFsAlign (align : Impl.Fs.Align) =
    if align.IsEmpty then None
    else
      Some { Direction=if align.Direction = Impl.Fs.AlignDir.Left then Left else Right
             Width=align.Width }

  let ofFsMt (tokens : Impl.Fs.Token seq) =
    tokens
    |> Seq.map (function
      | Impl.Fs.Token.TextToken (startIndex, text) -> Token.TextToken (startIndex, text)
      | Impl.Fs.Token.PropToken (startIndex, fsProp) ->
        let prop = { Name=fsProp.Name
                     CaptureHint=fromFsHint fsProp.Destr
                     Format=if isNull fsProp.Format then None else Some fsProp.Format
                     Align=fromFsAlign fsProp.Align
                     IsPositional=fsProp.IsPositional }
        Token.PropToken (startIndex, prop)
    )
    |> Seq.toList

/// Parses message templates from the different implementations in semi-compatible ways by 
/// using FsMessageTemplates.Token as the target type.
let parsedAs lang message (expectedTokens: Impl.Fs.Token seq) =
    let ignorePosAndStartIndex, parsed =
        match lang with
        | "C#" -> false, (Impl.Cs.parse message).Tokens |> Seq.map CsToFs.mttToToken |> List.ofSeq
        | "F#" -> false, (Impl.Fs.parse message).Tokens |> List.ofSeq
        | "F#MtParser" ->
          let tokens = ResizeArray<Impl.FsMt.ParserToken>()
          let foundText s = tokens.Add (Impl.FsMt.ParserToken.TextToken s)
          let foundProp p = tokens.Add (Impl.FsMt.ParserToken.PropToken p)
          FsMtParser.parseParts message foundText foundProp
          let fsTokens =
            tokens
            |> Seq.map (function
              | Impl.FsMt.ParserToken.TextToken s -> Impl.Fs.Token.TextToken(0, s)
              | Impl.FsMt.ParserToken.PropToken p ->
                let destr = match p.captureHint with
                            | Impl.FsMt.Destr.Structure -> Impl.Fs.Destr.Destructure
                            | Impl.FsMt.Destr.Stringify -> Impl.Fs.Destr.Stringify
                            | _ -> Impl.Fs.Destr.Default
                let align = match p.align with
                            | ai when ai.isEmpty -> Impl.Fs.Align.Empty
                            | ai ->
                              let dir =
                                if ai.direction = Impl.FsMt.AlignDir.Left then Impl.Fs.AlignDir.Left
                                else Impl.Fs.AlignDir.Right

                              Impl.Fs.Align(dir, ai.width)

                Impl.Fs.Token.PropToken(0, Impl.Fs.Prop(p.name, -1, destr, align, p.format)) )
            |> List.ofSeq
          true, fsTokens
        | other -> failwithf "unexpected lang '%s'" other

    let setStartIndexZeroIfIgnored (tokens : #seq<Impl.Fs.Token>) =
      if not ignorePosAndStartIndex then tokens
      else
        tokens |> Seq.map (function
          | Impl.Fs.Token.TextToken (i, t) -> Impl.Fs.Token.TextToken(0, t)
          | Impl.Fs.Token.PropToken (i, p) ->
            Impl.Fs.Token.PropToken(0, Impl.Fs.Prop(p.Name, -1, p.Destr, p.Align, p.Format)))

    let expected = expectedTokens |> setStartIndexZeroIfIgnored |> TestModel.ofFsMt
    let parsed = parsed |> TestModel.ofFsMt
    Xunit.Assert.Equal<TestModel.Token list> (expected, parsed)

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
    static member ParsedAs (lang, message,  expectedTokens: Impl.Fs.Token seq) =
        parsedAs lang message expectedTokens

    /// Captures properties from the C# or F# version in compatible ways.
    static member internal Capture(lang, template, values, ?maxDepth, ?additionalScalars, ?additionalDestrs) =
        let maxDepth = defaultArg maxDepth 10
        let additionalScalars = defaultArg additionalScalars Seq.empty<Type>
        let additionalDestrs = defaultArg additionalDestrs Seq.empty<Impl.Fs.Destructurer>
        match lang with
        | "C#" ->
            let mt = Impl.Cs.parse template
            let csDestrs = additionalDestrs |> Seq.map CsToFs.fsDestrToCsDestrPolicy
            let captured = CsToFs.CsMt.CaptureWith(maxDepth, additionalScalars, csDestrs, mt, values)
            captured |> Seq.map CsToFs.templateProperty |> Seq.toList
        | "F#" ->
            let mt = Impl.Fs.parse template
            let emptyKeepTrying = Unchecked.defaultof<Impl.Fs.TemplatePropertyValue>
            let tryScalars : Impl.Fs.Destructurer = fun r ->
                let exists = additionalScalars |> Seq.exists ((=) (r.Value.GetType()))
                if exists then Impl.Fs.ScalarValue(r.Value) else emptyKeepTrying
            let destrNoneForNull (r: Impl.Fs.DestrRequest) (d: Impl.Fs.Destructurer) =
                match d (Impl.Fs.DestrRequest (r.Destructurer, r.Value, 10, 1, hint=r.Hint)) with
                | tpv when tpv = Impl.Fs.TemplatePropertyValue.Empty -> None
                | tpv -> Some tpv
            let tryDestrs : Impl.Fs.Destructurer = fun r ->
                match additionalDestrs |> Seq.tryPick (destrNoneForNull r) with
                | None -> emptyKeepTrying
                | Some tpv -> tpv
            let destr = FsMessageTemplates.Capturing.createCustomDestructurer (Some tryScalars) (Some tryDestrs)
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
                              |> fun tpvl -> Impl.Cs.TemplatePropertyValueDictionary(tpvl)

            mt.Render(properties=propsByName, formatProvider=provider)
        | "F#" ->
            let mt = FsMessageTemplates.Parser.parse template
            let emptyKeepTrying = Unchecked.defaultof<Impl.Fs.TemplatePropertyValue>
            let tryScalars : Impl.Fs.Destructurer = fun r ->
                let exists = additionalScalars |> Seq.exists ((=) (r.Value.GetType()))
                if exists then Impl.Fs.ScalarValue(r.Value) else emptyKeepTrying
            let destrNoneForNull (r: Impl.Fs.DestrRequest) (d: Impl.Fs.Destructurer) =
                match d (Impl.Fs.DestrRequest (r.Destructurer, r.Value, 10, 1, hint=r.Hint)) with
                | tpv when tpv = Impl.Fs.TemplatePropertyValue.Empty -> None
                | tpv -> Some tpv
            let tryDestrs : Impl.Fs.Destructurer = fun r ->
                match additionalDestrs |> Seq.tryPick (destrNoneForNull r) with
                | None -> emptyKeepTrying
                | Some tpv -> tpv
            let destr = FsMessageTemplates.Capturing.createCustomDestructurer (Some tryScalars) (Some tryDestrs)
            let propsByName = FsMessageTemplates.Capturing.capturePropertiesCustom destr maxDepth mt values
                              |> Seq.map (fun tpv -> tpv.Name, tpv.Value)
                              |> dict
            let getValueByName name =
                let exists, value = propsByName.TryGetValue(name)
                if exists then value else Impl.Fs.TemplatePropertyValue.Empty
            use tw = new System.IO.StringWriter(formatProvider=provider)
            FsMessageTemplates.Formatting.formatCustom mt tw getValueByName
            let formatCustomOutput = tw.ToString()

            // if invariant and no custom scalars and types provided, verify 'format' gives the same result
            // if maxDepth is different, we also can't assert the other overloads produce the same output, as
            // only formatCustom allows this to change.
            if maxDepth=10 && additionalScalars = Seq.empty<Type> && additionalDestrs = Seq.empty<Impl.Fs.Destructurer> then
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
