module FsMessageTemplates.MessageTemplates

/// Describes the destructuring hint, parsed from within a <see cref="PropertyToken" />.
type Destructure = Default | Destructure | Stringify

/// Represents a property parsed from within a message template.
type PropertyTokenInfo = {
    /// The property name extracted from the template message.
    Name: string
    /// The destructuring hint.
    DestructuringHint: Destructure option
    /// The alignment string (e.g. ",-10")
    Align: string option
    /// The 'format' string (e.g. ":0000")
    Format: string option
    /// The index into the string where the first character of the token was found
    Index: int }

/// The index into the string where the first character of the token was found
type TextTokenInfo = {
    /// The literal text value.
    Text: string
    /// The index into the string where the first character of the token was found
    Index: int }

type Token =
| TextToken of TextTokenInfo
| PropertyToken of PropertyTokenInfo

/// A template, including the message and parsed properties.
type Template = {
    FormatString: string
    Tokens: Token list }

/// Parses a message template string.
val parse: templateString:string -> Template

/// Formats a message template as a string, replacing the properties
/// with the provided values.
val format: provider: System.IFormatProvider
            -> t:Template
            -> [<System.ParamArray>]values: obj[]
            -> string
