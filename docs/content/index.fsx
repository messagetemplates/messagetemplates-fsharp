(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.

#I "../../bin/MessageTemplates"
#I "../../bin/FsMessageTemplates"

(**
MessageTemplates
======================

Documentation

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The MessageTemplates library can be <a href="https://nuget.org/packages/MessageTemplates">installed from NuGet</a>:
      <pre>PM> Install-Package MessageTemplates</pre>
      OR
      <pre>PM> Install-Package FsMessageTemplates</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

# MessageTemplates

An implementation of named string replacements, which allows formatting, parsing, and capturing properties. MessageTemplates is compatible with the [Serilog](http://serilog.net/) [template format](https://github.com/serilog/serilog/wiki/Structured-Data).

The C# implementation was extracted from Serilog itself.

The F# implementation was designed to be copied (or [paket github referenced](http://fsprojects.github.io/Paket/github-dependencies.html)) into existing libraries or applications directly, however there may also be a Nuget package in the future.

### Message Template Syntax

Message templates are a superset of standard .NET format strings, so any format string acceptable to `string.Format()` will also be correctly processed by `MessageTemplates`.

* Property names are written between `{` and `}` brackets
* Brackets can be escaped by doubling them, e.g. `{{` will be rendered as `{`
* Formats that use numeric property names, like `{0}` and `{1}` exclusively, will be matched with the `Format` method's parameters by treating the property names as indexes; this is identical to `string.Format()`'s behaviour
* If any of the property names are non-numeric, then all property names will be matched from left-to-right with the `Format` method's parameters
* Property names may be prefixed with an optional operator, `@` or `$`, to control how the property is serialised
* Property names may be suffixed with an optional format, e.g. `:000`, to control how the property is rendered; these format strings behave exactly as their counterparts within the `string.Format()` syntax

Example
-------

This example demonstrates using a function defined in this sample library.

*)
#r "System.IO"
#r "System"
#r "MessageTemplates"

printfn "%s" (MessageTemplates.MessageTemplate.Format("test {this}", "success"))
printfn "%s" (MessageTemplates.MessageTemplate.Format("test {this:l}", "success"))

#r "FsMessageTemplates"
let invariant = System.Globalization.CultureInfo.InvariantCulture
open FsMessageTemplates.Formatting
printfn "%s" (sprintsm invariant "test {this:l}" [|"success"|])

(**
Some more info

Samples & documentation
-----------------------

The library comes with comprehensible documentation. 
It can include tutorials automatically generated from `*.fsx` files in [the content folder][content]. 
The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Apache 2.0 license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/adamchester/MessageTemplates/tree/master/docs/content
  [gh]: https://github.com/adamchester/MessageTemplates
  [issues]: https://github.com/adamchester/MessageTemplates/issues
  [readme]: https://github.com/adamchester/MessageTemplates/blob/master/README.md
  [license]: https://github.com/adamchester/MessageTemplates/blob/master/LICENSE.txt
*)
