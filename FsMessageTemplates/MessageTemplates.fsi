module FsMessageTemplates.MessageTemplates

/// A hint at how a property should be destructured.
type DestructureKind = Default = 0 | Stringify = 1 | Destructure = 2
type Direction = Left = 0 | Right = 1

[<Struct>]
type AlignInfo =
    new: Direction:Direction * Width:int -> AlignInfo
    member Direction : Direction
    member Width : int

/// Represents the details about property parsed from within a message template.
[<Struct>]
type PropertyData =
    new: name:string * pos:int option * destr:DestructureKind * align: AlignInfo option * format: string option -> PropertyData
    member Name:string
    member Pos:int option
    member Destr:DestructureKind
    member Align:AlignInfo option
    member Format:string option
    with
        static member Empty: PropertyData
        member IsPositional : bool

/// A token parsed from a message template.
type Token =
/// A piece of text within a message template.
| Text of startIndex:int * text:string
/// A property within a message template.
| Prop of startIndex:int * PropertyData

/// A template, including the message and parsed properties.
type Template = { FormatString: string; Tokens: Token list }

/// Parses a message template string.
val parse: template:string -> Template

/// Formats a message template as a string, replacing the properties
/// with the provided values.
val format: provider:System.IFormatProvider
            -> template:Template
            -> values:obj[]
            -> string

/// Captures the properties as they are provided to a message template.
val captureProperties: template:Template -> args:obj[] -> (PropertyData * obj) seq
