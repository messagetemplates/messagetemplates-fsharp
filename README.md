
# FsMessageTemplates

An implementation of {named} string replacements, which allows formatting, parsing, and capturing properties.

`FsMessageTemplates` is compatible with the [Message Templates Standard](http://messagetemplates.org/).

The F# implementation was designed to be paket-github referenced, or installed as a nuget package.

[![Build status](https://ci.appveyor.com/api/projects/status/e2y2xpegw081p0tl?svg=true)](https://ci.appveyor.com/project/adamchester/messagetemplates-fsharp)

### Samples

#### Format an F# Record
```fsharp
type User = { Id:int; Name:string }
format (parse "Hello, {@user}") [| {Id=1; Name="Adam"} |]
// > val it : string = "Hello, User { Id: 1, Name: "Adam" }"
```

### Message Template Syntax

[Message Templates](http://messagetemplates.org/) are a superset of standard .NET format strings, so any format string acceptable to `string.Format()` will also be correctly processed by `FsMessageTemplates`.

* Property names are written between `{` and `}` brackets
* Brackets can be escaped by doubling them, e.g. `{{` will be rendered as `{`
* Formats that use numeric property names, like `{0}` and `{1}` exclusively, will be matched with the `Format` method's parameters by treating the property names as indexes; this is identical to `string.Format()`'s behaviour
* If any of the property names are non-numeric, then all property names will be matched from left-to-right with the `Format` method's parameters
* Property names may be prefixed with an optional operator, `@` or `$`, to control how the property is serialised
* Property names may be suffixed with an optional format, e.g. `:000`, to control how the property is rendered; these format strings behave exactly as their counterparts within the `string.Format()` syntax
