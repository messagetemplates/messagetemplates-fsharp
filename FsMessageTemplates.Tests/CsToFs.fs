module FsTests.CsToFs

open FsMessageTemplates
open Tk

type Kvp<'a,'b> = System.Collections.Generic.KeyValuePair<'a,'b>

type FsMt = FsMessageTemplates.Template
type CsMt = MessageTemplates.MessageTemplate

type CsTextToken = MessageTemplates.Parsing.TextToken
type CsPropertyToken = MessageTemplates.Parsing.PropertyToken
type CsDestructuring = MessageTemplates.Parsing.Destructuring
type CsAlignmentDirection = MessageTemplates.Parsing.AlignmentDirection
type CsMessageTemplateToken = MessageTemplates.Parsing.MessageTemplateToken
type CsTemplateProperty = MessageTemplates.Structure.TemplateProperty
type CsTemplatePropertyValue = MessageTemplates.Structure.TemplatePropertyValue
type CsScalarValue = MessageTemplates.Structure.ScalarValue
type CsSequenceValue = MessageTemplates.Structure.SequenceValue
type CsStructureValue = MessageTemplates.Structure.StructureValue
type CsDictionaryValue = MessageTemplates.Structure.DictionaryValue

let (|Null|Value|) (x: _ System.Nullable) = if x.HasValue then Value x.Value else Null

/// Converts a C# TextToken to an F# Token.Text
let textToToken (tt: CsTextToken) = Token.Text(tt.StartIndex, tt.Text)

/// Converts a C# PropertyToken to an F# Token.Prop
let propToToken (pr: CsPropertyToken) =
    let pos = match pr.TryGetPositionalValue() with | true, i -> i | false, _ -> -1
    let destr = match pr.Destructuring with
                | CsDestructuring.Default -> DestrHint.Default
                | CsDestructuring.Destructure -> DestrHint.Destructure
                | CsDestructuring.Stringify -> DestrHint.Stringify
                | d -> failwithf "unknown destructure %A" d
    let getDirection d = match d with
                         | CsAlignmentDirection.Left -> Direction.Left
                         | CsAlignmentDirection.Right -> Direction.Right
                         | _ -> failwithf "unknown direction %A" d
    let align = match pr.Alignment with
                | Value v -> AlignInfo(getDirection v.Direction, v.Width)
                | Null _ -> AlignInfo.Empty
    Token.Prop(pr.StartIndex, PropertyToken(pr.PropertyName, pos, destr, align, pr.Format))

/// Converts a C# MessageTemplateToken to an F# Token
let mttToToken (mtt: CsMessageTemplateToken) : Token =
    match mtt with
    | :? CsPropertyToken as pt -> propToToken pt
    | :? CsTextToken as tt -> textToToken tt
    | _ -> failwithf "unknown token %A" mtt

/// Converts a C# TemplatePropertyValue to an F# TemplatePropertyValue
let rec templatePropertyValue (tpv: CsTemplatePropertyValue) : TemplatePropertyValue =
    match tpv with
    | :? MessageTemplates.Structure.ScalarValue as sv -> ScalarValue sv.Value
    | :? MessageTemplates.Structure.SequenceValue as sev -> SequenceValue (sev.Elements |> Seq.map templatePropertyValue |> Seq.toList)
    | :? MessageTemplates.Structure.DictionaryValue as dv ->
        let keyMap (kvp:Kvp<CsScalarValue, CsTemplatePropertyValue>) = templatePropertyValue kvp.Key
        let valueMap (kvp:Kvp<CsScalarValue, CsTemplatePropertyValue>) = templatePropertyValue kvp.Value
        DictionaryValue (dv.Elements |> Seq.map (fun kvp -> keyMap kvp, valueMap kvp) |> Seq.toList)
    | :? MessageTemplates.Structure.StructureValue as strv ->
        let structureValues = strv.Properties |> Seq.map (fun p -> PropertyNameAndValue(p.Name, templatePropertyValue p.Value)) |> Seq.toList
        StructureValue (strv.TypeTag, structureValues)
    | _ -> failwithf "unknown template property value type %A" tpv

let templateProperty (tp: CsTemplateProperty) = PropertyNameAndValue(tp.Name, (templatePropertyValue tp.Value))


/// Creates a C# TemplatePropertyValue from an F# TemplatePropertyValue
let rec createCsTpvFromFsTpv (fsTpv: TemplatePropertyValue) : CsTemplatePropertyValue =
    match fsTpv with
    | ScalarValue v -> CsScalarValue v :> CsTemplatePropertyValue
    | SequenceValue values -> CsSequenceValue(values |> List.map createCsTpvFromFsTpv) :> CsTemplatePropertyValue
    | StructureValue(typeTag, values) ->
        let csTemplateProperties =
            values |> List.map (fun fsPnV -> CsTemplateProperty(fsPnV.Name, createCsTpvFromFsTpv fsPnV.Value))
        CsStructureValue(csTemplateProperties, typeTag) :> CsTemplatePropertyValue
    | DictionaryValue(data) ->
        let fsTpvToCsScalar = function ScalarValue sv -> CsScalarValue(sv) | _ -> failwithf "cannot convert tpv to Scalar %A" fsTpv
        let csValues = data |> Seq.map (fun (k, v) -> Kvp(fsTpvToCsScalar k, createCsTpvFromFsTpv v))
        CsDictionaryValue csValues :> CsTemplatePropertyValue

/// Creates a C# ITemplatePropertyValueFactor from an F# Destructurer
let createTpvFactory (destr: Destructurer) : MessageTemplates.Core.ITemplatePropertyValueFactory =
    { new MessageTemplates.Core.ITemplatePropertyValueFactory with
          member __.CreatePropertyValue(value: obj, destructureObjects: bool) : MessageTemplates.Structure.TemplatePropertyValue = 
            let hint = if destructureObjects then DestrHint.Destructure else DestrHint.Default
            let req = DestructureRequest (destr, value, hint=hint)
            destr req |> createCsTpvFromFsTpv
    }

let fsDestrToCsDestrPolicy (destr: Destructurer) =
    { new MessageTemplates.Core.IDestructuringPolicy with
            member x.TryDestructure(value: obj, pvf: MessageTemplates.Core.ITemplatePropertyValueFactory, result: byref<MessageTemplates.Structure.TemplatePropertyValue>): bool = 
                let req = DestructureRequest(destr, value, hint=DestrHint.Destructure)
                let tpv = destr req |> createCsTpvFromFsTpv
                result <- tpv
                true
    }

/// Converts a C# IDestructuringPolicy to an F# Destructurer.
let toFsDestructurer (dp: MessageTemplates.Core.IDestructuringPolicy) : Destructurer =
    fun (req: DestructureRequest) ->
        let factory = createTpvFactory req.Destructurer
        let success, value = dp.TryDestructure (req.Value, factory) 
        if success then templatePropertyValue value
        else Unchecked.defaultof<TemplatePropertyValue>
