module MessageTemplates

type Property = {
    Name: string }
type Template = {
    FormatString: string
    Properties: Property list }

val parse: templateString:string -> Template
val format: Template -> [<System.ParamArray>]values:obj[] -> string
