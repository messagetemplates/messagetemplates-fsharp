module Tk

open FsMessageTemplates.MessageTemplates

// let td tindex raw = { Text=raw; StartIndex=tindex }
let text tindex raw = Token.Text(tindex, raw)
let desDef = DestrHint.Default
let prop tindex (_:string) name = Token.Prop(tindex, PropertyToken(name, None, desDef, None, None))
let propf tindex (_:string) name format = Token.Prop(tindex, PropertyToken(name, None, desDef, None, Some format))
let propd tindex (_:string) name = Token.Prop(tindex, PropertyToken(name, None, DestrHint.Destructure, None, None))
let propds tindex (_:string) name = Token.Prop(tindex, PropertyToken(name, None, DestrHint.Stringify, None, None))
let propar tindex (_:string) name rightWidth = Token.Prop(tindex, PropertyToken(name, None, desDef, Some (AlignInfo(Direction.Right, rightWidth)), None))
let propal tindex (_:string) name leftWidth = Token.Prop(tindex, PropertyToken(name, None, desDef, Some (AlignInfo(Direction.Left, leftWidth)), None))
let propp tindex num = Token.Prop(tindex, PropertyToken(string num, Some num, desDef, None, None))
