module Tk

open FsMessageTemplates.MessageTemplates

// let td tindex raw = { Text=raw; StartIndex=tindex }
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
let propal tindex (_:string) name leftWidth = Token.Prop(tindex, PropertyToken(name, emptyPos, desDef, AlignInfo(Direction.Left, leftWidth), emptyFormat))
let propp tindex num = Token.Prop(tindex, PropertyToken(string num, num, desDef, emptyAlign, emptyFormat))
