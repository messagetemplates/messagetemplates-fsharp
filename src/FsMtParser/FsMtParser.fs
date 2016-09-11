module FsMtParser

open System.Text

type Property(name : string, format : string) =
  static let emptyInstance = Property("", null)
  static member empty = emptyInstance
  member x.name = name
  member x.format = format
  member internal x.AppendPropertyString(sb : StringBuilder, ?replacementName) =
    sb.Append("{")
      .Append(defaultArg replacementName name)
      .Append(match x.format with null | "" -> "" | _ -> ":" + x.format)
      .Append("}")
  override x.ToString() = x.AppendPropertyString(StringBuilder()).ToString()

module internal ParserBits =

  let inline isLetterOrDigit c = System.Char.IsLetterOrDigit c
  let inline isValidInPropName c = c = '_' || System.Char.IsLetterOrDigit c
  let inline isValidInFormat c = c <> '}' && (c = ' ' || isLetterOrDigit c || System.Char.IsPunctuation c)
  let inline isValidCharInPropTag c = c = ':' || isValidInPropName c || isValidInFormat c

  [<Struct>]
  type Range(startIndex : int, endIndex : int) =
    member inline x.start = startIndex
    member inline x.``end`` = endIndex
    member inline x.length = (endIndex - startIndex) + 1
    member inline x.getSubstring (s : string) = s.Substring(startIndex, x.length)
    member inline x.isEmpty = startIndex = -1 && endIndex = -1
    static member inline substring (s : string, startIndex, endIndex) = s.Substring(startIndex, (endIndex - startIndex) + 1)
    static member inline empty = Range(-1, -1)

  let inline tryGetFirstCharInRange predicate (s : string) (range : Range) =
    let rec go i =
      if i > range.``end`` then -1
      else if not (predicate s.[i]) then go (i+1) else i
    go range.start

  let inline tryGetFirstChar predicate (s : string) first =
    tryGetFirstCharInRange predicate s (Range(first, s.Length - 1))

  let inline hasAnyInRange predicate (s : string) (range : Range) =
    match tryGetFirstChar (predicate) s range.start with
    | -1 ->
      false
    | i ->
      i <= range.``end``

  let inline hasAny predicate (s : string) = hasAnyInRange predicate s (Range(0, s.Length - 1))
  let inline indexOfInRange s range c = tryGetFirstCharInRange ((=) c) s range

  let inline tryGetPropInRange (template : string) (within : Range) : Property =
    // Attempts to validate and parse a property token within the specified range inside
    // the template string. If the property insides contains any invalid characters,
    // then the `Property.Empty' instance is returned (hence the name 'try')
    let nameRange, formatRange =
      match indexOfInRange template within ':' with
      | -1 ->
        within, Range.empty // no format
      | formatIndex ->
        Range(within.start, formatIndex-1), Range(formatIndex+1, within.``end``) // has format part
    let propertyName = nameRange.getSubstring template
    if propertyName = "" || (hasAny (not<<isValidInPropName) propertyName) then
      Property.empty
    elif (not formatRange.isEmpty) && (hasAnyInRange (not<<isValidInFormat) template formatRange) then
      Property.empty
    else
      let format = if formatRange.isEmpty then null else formatRange.getSubstring template
      Property(propertyName, format)

  let findNextNonPropText (startAt : int) (template : string) (foundText : string->unit) : int =
    // Finds the next text token (starting from the 'startAt' index) and returns the next character
    // index within the template string. If the end of the template string is reached, or the start
    // of a property token is found (i.e. a single { character), then the 'consumed' text is passed
    // to the 'foundText' method, and index of the next character is returned.
    let rec go i =
      if i >= template.Length then template.Length
      else
        match template.[i] with
        | '{' ->
          if (i+1) < template.Length && template.[i+1] = '{' then go (i+2) else i
        | '}' when (i+1) < template.Length && template.[i+1] = '}' ->
          go (i+2)
        | _ ->
          go (i+1)
    let nextIndex = go startAt
    if (nextIndex > startAt) then
      foundText (Range.substring(template, startAt, nextIndex - 1))
    nextIndex

  let findPropOrText (start : int) (template : string)
                      (foundText : string -> unit)
                      (foundProp : Property -> unit) : int =
    // Attempts to find the indices of the next property in the template
    // string (starting from the 'start' index). Once the start and end of
    // the property token is known, it will be further validated (by the
    // tryGetPropInRange method). If the range turns out to be invalid, it's
    // not a property token, and we return it as text instead. We also need
    // to handle some special case here: if the end of the string is reached,
    // without finding the close brace (we just signal 'foundText' in that case).
    let nextInvalidCharIndex =
      match tryGetFirstChar (not << isValidCharInPropTag) template (start+1) with
      | -1 ->
        template.Length
      | idx ->
        idx

    if nextInvalidCharIndex = template.Length || template.[nextInvalidCharIndex] <> '}' then
      foundText (Range.substring(template, start, (nextInvalidCharIndex - 1)))
      nextInvalidCharIndex
    else
      let nextIndex = nextInvalidCharIndex + 1
      let propInsidesRng = Range(start + 1, nextIndex - 2)
      match tryGetPropInRange template propInsidesRng with
      | prop when not (obj.ReferenceEquals(prop, Property.empty)) ->
        foundProp prop
      | _ ->
        foundText (Range.substring(template, start, (nextIndex - 1)))
      nextIndex

/// Parses template strings such as "Hello, {PropertyWithFormat:##.##}"
/// and calls the 'foundTextF' or 'foundPropF' functions as the text or
/// property tokens are encountered.
let parseParts (template : string) foundTextF foundPropF =
  let tlen = template.Length
  let rec go start =
    if start >= tlen then ()
    else match ParserBits.findNextNonPropText start template foundTextF with
          | next when next <> start ->
            go next
          | _ ->
            go (ParserBits.findPropOrText start template foundTextF foundPropF)
  go 0
