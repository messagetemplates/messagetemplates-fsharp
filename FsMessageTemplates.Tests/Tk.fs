module Tk

open FsMessageTemplates.MessageTemplates

let PropertyNameAndValue (name:string, value:TemplatePropertyValue) = { Name=name; Value=value }
let text tindex raw = Token.Text(tindex, raw)
let desDef = DestrHint.Default
let emptyAlign = AlignInfo.Empty
let emptyPos = -1
let emptyFormat = null
let prop tindex (_:string) name = Token.Prop(tindex, PropertyToken(name, emptyPos, desDef, emptyAlign, emptyFormat))
let propf tindex (_:string) name format = Token.Prop(tindex, PropertyToken(name, emptyPos, desDef, emptyAlign, format))
let propd tindex (_:string) name = Token.Prop(tindex, PropertyToken(name, emptyPos, DestrHint.Destructure, emptyAlign, emptyFormat))
let propds tindex (_:string) name = Token.Prop(tindex, PropertyToken(name, emptyPos, DestrHint.Stringify, emptyAlign, emptyFormat))
let propar tindex (_:string) name rightWidth = Token.Prop(tindex, PropertyToken(name, emptyPos, desDef, AlignInfo(Direction.Right, rightWidth), emptyFormat))
let proparf tindex (_:string) name rightWidth format = Token.Prop(tindex, PropertyToken(name, emptyPos, desDef, AlignInfo(Direction.Right, rightWidth), format))
let propal tindex (_:string) name leftWidth = Token.Prop(tindex, PropertyToken(name, emptyPos, desDef, AlignInfo(Direction.Left, leftWidth), emptyFormat))
let propalf tindex (_:string) name leftWidth format = Token.Prop(tindex, PropertyToken(name, emptyPos, desDef, AlignInfo(Direction.Left, leftWidth), format))
let propp tindex num = Token.Prop(tindex, PropertyToken(string num, num, desDef, emptyAlign, emptyFormat))
let proppalf tindex num leftWidth format = Token.Prop(tindex, PropertyToken(string num, num, desDef, AlignInfo(Direction.Left, leftWidth), format))
let propparf tindex num rightWidth format = Token.Prop(tindex, PropertyToken(string num, num, desDef, AlignInfo(Direction.Right, rightWidth), format))
