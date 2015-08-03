module FsMessageTemplates.MessageTemplates

/// A hint at how a property should be destructured.
type DestructureKind = Default = 0 | Stringify = 1 | Destructure = 2

/// The alignment direction.
type Direction = Left = 0 | Right = 1

/// Represents the aligment information within a message template.
[<Struct>]
type AlignInfo =
    new: Direction:Direction * Width:int -> AlignInfo
    member Direction : Direction
    member Width : int

/// Represents the details about property parsed from within a message template.
[<Struct>]
type PropertyToken =
    /// Constructs a new instance of a template property.
    new: name:string
            * pos:int option
            * destr:DestructureKind
            * align: AlignInfo option
            * format: string option
            -> PropertyToken
    /// The name of the property.
    member Name:string
    /// If the property was positional (i.e. {0} or {1}, instead of {name}), this
    /// is the position number.
    member Pos:int option
    /// The destructuring hint (i.e. if {@name} was used then Destructure, if {$name}
    /// was used, then Stringify).
    member Destr:DestructureKind
    /// The alignment information (i.e. if {@name,-10} was parsed from the template, this
    /// would be AlignInfo(Direction.Right, 10)).
    member Align:AlignInfo option
    /// The format information (i.e. if {@name:0,000} was parsed from the template, this
    /// would be the string "0,000").
    member Format:string option
    with
        static member Empty: PropertyToken
        /// When the property is positional (i.e. if {0}, {1}, etc was used instead of a
        /// property name, this returns true. Get the postion number from the Pos field.
        member IsPositional : bool

/// A token parsed from a message template.
type Token =
/// A piece of text within a message template.
| Text of startIndex:int * text:string
/// A property within a message template.
| Prop of startIndex:int * PropertyToken

/// A template, including the message and parsed properties.
type Template = { FormatString: string; Tokens: Token list }

/// A simple value type.
type Scalar =
| [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
  Null
| Bool of bool | Char of char | Byte of byte
| Int16 of int16 | UInt16 of uint16
| Int32 of int32 | UInt32 of uint32
| Int64 of int64 | UInt64 of uint64
| Single of single | Double of double
| Decimal of decimal | String of string
| DateTime of System.DateTime | DateTimeOffset of System.DateTimeOffset
| TimeSpan of System.TimeSpan | Guid of System.Guid | Uri of System.Uri
| Custom of obj // Is this necessary? Looks like C# supports it via destr. policies?

/// A key and value pair, used as part of <see cref="TemplatePropertyValue.DictionaryValue" />.
type ScalarKeyValuePair = Scalar * obj

/// Describes the kinds of destructured property values captured from a message template.
type TemplatePropertyValue =
| ScalarValue of Scalar
| SequenceValue of TemplatePropertyValue seq
| StructureValue of typeTag:string option * values:(string*TemplatePropertyValue) list
| DictionaryValue of data: ScalarKeyValuePair seq

/// A property and it's associated destructured value.
type PropertyNameAndValue = string * TemplatePropertyValue

/// A function that attempts to destructure an object into a 
/// more friendly (and immutable) <see cref="TemplatePropertyValue" />.
type Destructurer = DestructureRequest -> PropertyNameAndValue option
and DestructureRequest = { tryDestructure:Destructurer; Kind:DestructureKind; Value:obj }

/// Parses a message template string.
val parse: template:string -> Template

/// Formats a message template as a string, replacing the properties
/// with the provided values.
val format: provider:System.IFormatProvider
            -> template:Template
            -> values:obj[]
            -> string

/// Extracts the properties for a template from the array of objects.
val captureProperties: destructure: Destructurer
                       -> template:Template
                       -> args:obj[]
                       -> PropertyNameAndValue seq
