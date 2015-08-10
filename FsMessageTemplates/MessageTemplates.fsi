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
[<Struct; StructuralEquality; StructuralComparison>]
type Template =
    val Tokens : Token list
    val FormatString : string
    val Properties : PropertyToken list
    val internal Named : PropertyToken list
    val internal PositionalsByPos : PropertyToken list

/// A key and value pair, used as part of <see cref="TemplatePropertyValue.DictionaryValue" />.
type ScalarKeyValuePair = obj * TemplatePropertyValue
/// Describes the kinds of destructured property values captured from a message template.
and TemplatePropertyValue =
| ScalarValue of obj
| SequenceValue of TemplatePropertyValue seq
| StructureValue of typeTag:string option * values:PropertyNameAndValue list
| DictionaryValue of data: ScalarKeyValuePair seq
/// A property and it's associated destructured value.
and PropertyNameAndValue = string * TemplatePropertyValue

/// A function that attempts to destructure a property and value object into a 
/// more friendly (and immutable) <see cref="TemplatePropertyValue" />. This returns
/// None if the conversion failed, otherwise Some.
type Destructurer = DestructureRequest -> TemplatePropertyValue option
and DestructureRequest = {
    Kind: DestructureKind
    Value: obj
    It: Destructurer }

/// Parses a message template string.
val parse: template:string -> Template

/// Formats a message template as a string, replacing the properties
/// with the provided values.
val format: provider:System.IFormatProvider
            -> template:Template
            -> values:obj[]
            -> string

/// Extracts the properties for a template from the array of objects.
val captureProperties: template:Template -> args:obj[] -> PropertyNameAndValue seq

/// Extracts the properties from a message template and the array of objects.
val captureMessageProperties: template:string -> args:obj[] -> PropertyNameAndValue seq

/// Prints the message template to a string builder.
val bprintn: sb:System.Text.StringBuilder -> template:string -> args:obj[] -> unit

/// Prints the message template to a string.
val sfprintn: provider:System.IFormatProvider -> template:string -> args:obj[] -> string

/// Prints the message template a text writer.
val fprintn: tw:System.IO.TextWriter -> template:string -> args:obj[] -> unit
