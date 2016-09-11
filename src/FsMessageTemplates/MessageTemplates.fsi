namespace FsMessageTemplates

/// A hint at how a property should be destructured. The '@' character means 
/// 'Destructure' whereas the '$' means stringify.
type DestrHint = Default = 0 | Stringify = 1 | Destructure = 2

/// The alignment direction.
type Direction = Left = 0 | Right = 1

/// Represents the aligment information within a message template.
[<Struct>]
type AlignInfo =
    new: Direction:Direction * Width:int -> AlignInfo
    member Direction : Direction
    member Width : int
    member IsEmpty: bool
    member internal IsValid: bool
    val private _direction:Direction
    val private _width:int
    static member Empty: AlignInfo
    static member internal Invalid: AlignInfo

/// Represents the details about property parsed from within a message template.
[<Struct>]
type Property =
    /// Constructs a new instance of a template property.
    new: name:string
            * pos:int
            * destr:DestrHint
            * align: AlignInfo
            * format: string
            -> Property
    /// The name of the property.
    member Name:string
    /// If the property was positional (i.e. {0} or {1}, instead of {name}), this
    /// is the position number.
    member Pos:int
    /// The destructuring hint (i.e. if {@name} was used then Destructure, if {$name}
    /// was used, then Stringify).
    member Destr:DestrHint
    /// The alignment information (i.e. if {@name,-10} was parsed from the template, this
    /// would be AlignInfo(Direction.Right, 10)).
    member Align:AlignInfo
    /// The format information (i.e. if {@name:0,000} was parsed from the template, this
    /// would be the string "0,000").
    member Format:string
    /// When the property is positional (i.e. if {0}, {1}, etc was used instead of a
    /// property name, this returns true. Get the postion number from the Pos field.
    member IsPositional : bool

/// A token parsed from a message template.
type Token =
/// A piece of text within a message template.
| TextToken of startIndex: int * text: string
/// A property within a message template.
| PropToken of startIndex: int * prop: Property

/// A template, including the message and parsed properties.
[<Class>]
type Template =
    new: formatString:string * tokens: Token[] * isNamed:bool * properties:Property[] -> Template
    member Tokens : Token seq
    member FormatString : string
    member Properties : Property seq
    member internal Named : Property []
    member internal Positionals : Property []

/// A key and value pair, used as part of <see cref="TemplatePropertyValue.DictionaryValue" />.
type ScalarKeyValuePair = TemplatePropertyValue * TemplatePropertyValue
/// Describes the kinds of destructured property values that can be
/// captured from a message template.
and TemplatePropertyValue =
| ScalarValue of obj
| SequenceValue of TemplatePropertyValue list
| StructureValue of typeTag:string * values:PropertyNameAndValue list
| DictionaryValue of data: ScalarKeyValuePair list
    static member Empty : TemplatePropertyValue
/// A property and it's associated destructured value.
and PropertyNameAndValue = { Name:string; Value:TemplatePropertyValue }

/// A function that attempts to destructure a property and value object into a 
/// more friendly (and immutable) <see cref="TemplatePropertyValue" />. This returns
/// Unchecked.defaultOf TemplatePropertyValue (i.e. null; aka TemplatePropertyValue.Empty)
/// if the destructuring was not possible.
type Destructurer = DestructureRequest -> TemplatePropertyValue
and
    /// Describes a request for an object to be destructured into a
    /// TemplatePropertyValue.
    [<Class; NoEquality; NoComparison>]
    DestructureRequest =
        new: destructurer:Destructurer * value:obj * maxDepth:int * currentDepth:int * hint:DestrHint -> DestructureRequest
        member Hint: DestrHint
        member Value: obj
        member Destructurer: Destructurer
        member TryAgainWithValue: newValue:obj -> TemplatePropertyValue

module Parser =
    /// Parses a message template string.
    val parse: template:string -> Template

module Capturing =
    /// Creates a customised default destructurer which optionally
    /// specifies different scalar and object destructurers. If the
    /// caller gave None for both, the destructurer returned would be
    /// the same as the default built-in one. A scalar destructurer 
    /// must return a TemplatePropertyValue.ScalarValue (or Empty),
    /// whereas the object destructurer is free to return any kind
    /// (or Empty).
    val createCustomDestructurer : tryScalars: Destructurer option
                                   -> tryObjects: Destructurer option
                                   -> Destructurer
    
    /// Provides better support for destructuring F# types like
    /// Discriminated Unions, Tuples, and Lists.
    val builtInFSharpTypesDestructurer : Destructurer

    /// Extracts the properties for a template from the array of objects.
    val captureProperties: template:Template -> args:obj[] -> PropertyNameAndValue seq

    /// Extracts the properties from a message template and the array of objects.
    val captureMessageProperties: template:string -> args:obj[] -> PropertyNameAndValue seq

    /// "Captures" properties for the template using the provided Destructurer,
    /// Template, and args. If the template has *all* positional properties, the
    /// positional indexes are used to match the args to the template properties.
    /// Otherwise, arguments are matched left-to-right with the properties, and
    /// any extra (unmatched) properties are ignored.
    val capturePropertiesCustom:
        destr:Destructurer -> maxDepth:int -> template:Template -> args:obj[] -> PropertyNameAndValue seq

module Formatting =
    /// Formats and appends the template message to a TextWriter, using the provided
    /// function to look up each TemplatePropertyValue for each property name.
    val formatCustom: template:Template
                        -> tw:System.IO.TextWriter
                        -> getValueByName: (string -> TemplatePropertyValue)
                        -> unit

    /// Formats a message template as a string, replacing the properties
    /// with the provided values.
    val format: template:Template -> values:obj[] -> string

    /// Prints the message template to a string builder.
    val bprintsm: sb:System.Text.StringBuilder -> template:string -> args:obj[] -> unit

    /// Prints the message template to a string.
    val sprintsm: provider:System.IFormatProvider -> template:string -> args:obj[] -> string

    /// Prints the message template a text writer.
    val fprintsm: tw:System.IO.TextWriter -> template:string -> args:obj[] -> unit

    /// Prints the message template to a string builder.
    val bprintm: template:Template -> sb:System.Text.StringBuilder -> args:obj[] -> unit

    /// Prints the message template to a string.
    val sprintm: template:Template -> provider:System.IFormatProvider -> args:obj[] -> string

    /// Prints the message template a text writer.
    val fprintm: template:Template -> tw:System.IO.TextWriter -> args:obj[] -> unit
