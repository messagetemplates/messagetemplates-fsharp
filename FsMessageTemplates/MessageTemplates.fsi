module FsMessageTemplates.MessageTemplates

/// A hint at how a property should be destructured.
type DestructureKind = Default = 0 | Stringify = 1 | Destructure = 2
type TokenData = { StartIndex:int; Text: string }
type Direction = Left = 0 | Right = 1
type AlignInfo = { Direction: Direction; Width: int }
/// Represents the details about property parsed from within a message template.
type PropertyData = { Name: string; Pos: int option; Destr: DestructureKind
                      Align: AlignInfo option; Format: string option }
    with static member Empty: PropertyData

/// A token parsed from a message template.
type Token =
/// A piece of text within a message template.
| Text of TokenData
/// A property within a message template.
| Prop of TokenData * PropertyData

/// A template, including the message and parsed properties.
type Template = { FormatString: string; Tokens: Token list }

/// A slightly more succicnt way to describe the tokens
module Tk =
    /// Create a text token
    val text: tindex:int -> raw:string -> Token
    /// Create a named property
    val prop: tindex:int -> raw:string -> name:string -> Token
    /// Create a property with a custom format string
    val propf: tindex:int -> raw:string -> name:string -> format:string -> Token
    /// Create a property with a destructure hint (@)
    val propd: tindex:int -> raw:string -> name:string -> Token
    /// Create a property with a destructure hint to stringify ($)
    val propds: tindex:int -> raw:string -> name:string -> Token
    /// Create a property with alignment-right width
    val propar: tindex:int -> raw:string -> name:string -> rightWidth:int -> Token
    /// Create a property with alignment-left width
    val propal: tindex:int -> raw:string -> name:string -> leftWidth:int -> Token
    /// Create a 'positional' property token (e.g. "{0}" or "{1}")
    val propp: tindex:int -> num:int -> Token

/// Parses a message template string.
val parse: templateString:string -> Template

/// Formats a message template as a string, replacing the properties
/// with the provided values.
val format: provider:System.IFormatProvider
            -> template:Template
            -> values:obj[]
            -> string
