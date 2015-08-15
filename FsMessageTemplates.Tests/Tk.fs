module Tk

open FsMessageTemplates.MessageTemplates

// let td tindex raw = { Text=raw; StartIndex=tindex }
let text tindex raw = Token.Text(tindex, raw)
let desDef = DestrHint.Default
let emptyAlign = AlignInfo.Empty
let prop tindex (_:string) name = Token.Prop(tindex, PropertyToken(name, None, desDef, emptyAlign, None))
let propf tindex (_:string) name format = Token.Prop(tindex, PropertyToken(name, None, desDef, emptyAlign, Some format))
let propd tindex (_:string) name = Token.Prop(tindex, PropertyToken(name, None, DestrHint.Destructure, emptyAlign, None))
let propds tindex (_:string) name = Token.Prop(tindex, PropertyToken(name, None, DestrHint.Stringify, emptyAlign, None))
let propar tindex (_:string) name rightWidth = Token.Prop(tindex, PropertyToken(name, None, desDef, AlignInfo(Direction.Right, rightWidth), None))
let propal tindex (_:string) name leftWidth = Token.Prop(tindex, PropertyToken(name, None, desDef, AlignInfo(Direction.Left, leftWidth), None))
let propp tindex num = Token.Prop(tindex, PropertyToken(string num, Some num, desDef, emptyAlign, None))
