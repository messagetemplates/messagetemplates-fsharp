module FsMessageTemplates.MessageTemplates

/// A hint at how a property should be destructured.
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
    static member Invalid: AlignInfo

/// Represents the details about property parsed from within a message template.
[<Struct>]
type PropertyToken =
    /// Constructs a new instance of a template property.
    new: name:string
            * pos:int option
            * destr:DestrHint
            * align: AlignInfo
            * format: string option
            -> PropertyToken
    /// The name of the property.
    member Name:string
    /// If the property was positional (i.e. {0} or {1}, instead of {name}), this
    /// is the position number.
    member Pos:int option
    /// The destructuring hint (i.e. if {@name} was used then Destructure, if {$name}
    /// was used, then Stringify).
    member Destr:DestrHint
    /// The alignment information (i.e. if {@name,-10} was parsed from the template, this
    /// would be AlignInfo(Direction.Right, 10)).
    member Align:AlignInfo
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
[<Class>]
type Template =
    member Tokens : Token seq
    member FormatString : string
    member Properties : PropertyToken []
    member internal Named : PropertyToken []
    member internal PositionalsByPos : PropertyToken []

/// A key and value pair, used as part of <see cref="TemplatePropertyValue.DictionaryValue" />.
type ScalarKeyValuePair = TemplatePropertyValue * TemplatePropertyValue
/// Describes the kinds of destructured property values captured from a message template.
and TemplatePropertyValue =
| ScalarValue of obj
| SequenceValue of TemplatePropertyValue list
| StructureValue of typeTag:string option * values:PropertyNameAndValue list
| DictionaryValue of data: ScalarKeyValuePair list
/// A property and it's associated destructured value.
and PropertyNameAndValue = string * TemplatePropertyValue

/// A function that attempts to destructure a property and value object into a 
/// more friendly (and immutable) <see cref="TemplatePropertyValue" />. This returns
/// None if the conversion failed, otherwise Some.
type Destructurer = DestructureRequest -> TemplatePropertyValue
and
    [<Struct; NoEquality; NoComparison>]
    DestructureRequest =
        new: hint:DestrHint * value:obj * destr:Destructurer -> DestructureRequest
        member Hint: DestrHint
        member Value: obj
        member Destr: Destructurer

type PropertyAndValue = PropertyToken * TemplatePropertyValue

/// Parses a message template string.
val parse: template:string -> Template

/// Extracts the properties for a template from the array of objects.
val captureProperties: template:Template -> args:obj[] -> PropertyNameAndValue seq

/// Extracts the properties from a message template and the array of objects.
val captureMessageProperties: template:string -> args:obj[] -> PropertyNameAndValue seq

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
