module FsTests.CsToFs

open FsMessageTemplates.MessageTemplates

type CsTextToken = MessageTemplates.Parsing.TextToken
type CsPropertyToken = MessageTemplates.Parsing.PropertyToken
type CsDestructuring = MessageTemplates.Parsing.Destructuring
type CsAlignmentDirection = MessageTemplates.Parsing.AlignmentDirection
type CsMessageTemplateToken = MessageTemplates.Parsing.MessageTemplateToken

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
