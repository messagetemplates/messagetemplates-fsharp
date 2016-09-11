module Tk

open FsMessageTemplates

let PropertyNameAndValue (name:string, value:TemplatePropertyValue) = { Name=name; Value=value }
let text tindex raw = Token.TextToken(tindex, raw)
let desDef = DestrHint.Default
let emptyAlign = AlignInfo.Empty
let emptyPos = -1
let emptyFormat = null
let prop tindex (_:string) name = PropToken(tindex, Property(name, emptyPos, desDef, emptyAlign, emptyFormat))
let propf tindex (_:string) name format = PropToken(tindex, Property(name, emptyPos, desDef, emptyAlign, format))
let propd tindex (_:string) name = PropToken(tindex, Property(name, emptyPos, DestrHint.Destructure, emptyAlign, emptyFormat))
let propds tindex (_:string) name = PropToken(tindex, Property(name, emptyPos, DestrHint.Stringify, emptyAlign, emptyFormat))
let propar tindex (_:string) name rightWidth = PropToken(tindex, Property(name, emptyPos, desDef, AlignInfo(Direction.Right, rightWidth), emptyFormat))
let proparf tindex (_:string) name rightWidth format = PropToken(tindex, Property(name, emptyPos, desDef, AlignInfo(Direction.Right, rightWidth), format))
let propal tindex (_:string) name leftWidth = PropToken(tindex, Property(name, emptyPos, desDef, AlignInfo(Direction.Left, leftWidth), emptyFormat))
let propalf tindex (_:string) name leftWidth format = PropToken(tindex, Property(name, emptyPos, desDef, AlignInfo(Direction.Left, leftWidth), format))
let propp tindex num = PropToken(tindex, Property(string num, num, desDef, emptyAlign, emptyFormat))
let proppalf tindex num leftWidth format = PropToken(tindex, Property(string num, num, desDef, AlignInfo(Direction.Left, leftWidth), format))
let propparf tindex num rightWidth format = PropToken(tindex, Property(string num, num, desDef, AlignInfo(Direction.Right, rightWidth), format))
