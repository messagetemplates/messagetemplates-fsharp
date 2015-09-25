namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsMessageTemplates")>]
[<assembly: AssemblyProductAttribute("MessageTemplates")>]
[<assembly: AssemblyDescriptionAttribute("The ability to format named string values, and capture the properties")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
