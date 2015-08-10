module FsTests.CsToFs

open FsMessageTemplates.MessageTemplates

type Kvp<'a,'b> = System.Collections.Generic.KeyValuePair<'a,'b>

type CsTextToken = MessageTemplates.Parsing.TextToken
type CsPropertyToken = MessageTemplates.Parsing.PropertyToken
type CsDestructuring = MessageTemplates.Parsing.Destructuring
type CsAlignmentDirection = MessageTemplates.Parsing.AlignmentDirection
type CsMessageTemplateToken = MessageTemplates.Parsing.MessageTemplateToken
type CsTemplateProperty = MessageTemplates.Structure.TemplateProperty
type CsTemplatePropertyValue = MessageTemplates.Structure.TemplatePropertyValue
type CsScalarValue = MessageTemplates.Structure.ScalarValue
type CsSequenceValue = MessageTemplates.Structure.SequenceValue
type CsDictionaryValue = MessageTemplates.Structure.DictionaryValue

let (|Null|Value|) (x: _ System.Nullable) = if x.HasValue then Value x.Value else Null
let textToToken (tt: CsTextToken) = Token.Text(tt.StartIndex, tt.Text)
let propToToken (pr: CsPropertyToken) =
    let pos = match pr.TryGetPositionalValue() with
              | true, i -> Some i
              | false, _ -> None
    let destr = match pr.Destructuring with
                | CsDestructuring.Default -> DestructureKind.Default
                | CsDestructuring.Destructure -> DestructureKind.Destructure
                | CsDestructuring.Stringify -> DestructureKind.Stringify
                | d -> failwithf "unknown destructure %A" d
    let getDirection d = match d with
                         | CsAlignmentDirection.Left -> Direction.Left
                         | CsAlignmentDirection.Right -> Direction.Right
                         | _ -> failwithf "unknown direction %A" d
    let align = match pr.Alignment with
                | Value v -> Some (AlignInfo(getDirection v.Direction, v.Width))
                | Null _ -> None
    let format = match pr.Format with | null -> None | s -> Some s
    Token.Prop(pr.StartIndex, PropertyToken(pr.PropertyName, pos, destr, align, format))

let mttToToken (mtt: CsMessageTemplateToken) : Token =
    match mtt with
    | :? CsPropertyToken as pt -> propToToken pt
    | :? CsTextToken as tt -> textToToken tt
    | _ -> failwithf "unknown token %A" mtt

open System
type DtOffset = System.DateTimeOffset

let rec templatePropertyValue (tpv: CsTemplatePropertyValue) : TemplatePropertyValue =
    match tpv with
    | :? MessageTemplates.Structure.ScalarValue as sv -> ScalarValue sv.Value
    | :? MessageTemplates.Structure.SequenceValue as sev -> SequenceValue (sev.Elements |> Seq.map templatePropertyValue |> Seq.cache)
    | :? MessageTemplates.Structure.DictionaryValue as dv ->
        let keyMap (kvp:Kvp<CsScalarValue, CsTemplatePropertyValue>) = kvp.Key.Value
        let valueMap (kvp:Kvp<CsScalarValue, CsTemplatePropertyValue>) = templatePropertyValue kvp.Value
        DictionaryValue (dv.Elements |> Seq.map (fun kvp -> keyMap kvp, valueMap kvp))
    | :? MessageTemplates.Structure.StructureValue as strv ->
        let structureValues = strv.Properties |> Seq.map (fun p -> p.Name, templatePropertyValue p.Value) |> Seq.toList
        let typeTagOption = if String.IsNullOrEmpty(strv.TypeTag) then None else Some strv.TypeTag
        StructureValue (typeTagOption, structureValues)
    | _ -> failwithf "unknown template property value type %A" tpv

let templateProperty (tp: CsTemplateProperty) : PropertyNameAndValue = tp.Name, (templatePropertyValue tp.Value)