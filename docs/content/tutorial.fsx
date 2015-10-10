(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/MessageTemplates"
#I "../../bin/FsMessageTemplates"
#r "System.IO"

#r "FsMessageTemplates"
#load "../../tests/FsMessageTemplates.Tests/Tk.fs"
let invariantCulture = System.Globalization.CultureInfo.InvariantCulture :> System.IFormatProvider

(**
Introducing Message Templates
=============================

## Using FsMessageTemplates from F#
*)

(*** hide ***)
open FsMessageTemplates

(** First, we delcare some types for demo purposes *)
type User = { Id:int; Name:string }
type Manager = { Name:string; Reports:int }
type Person = User of User | Manager of Manager
    with override this.ToString() = match this with User u -> u.Name | Manager m -> m.Name

(** Create a message template *)
let template = Parser.parse "Hello, {@person}"
// val template : Template =
//  T (tokens = [|Text (0,"Hello, "); Prop (7,{@person})|])

(** Format the template using an F# record argument: *)
let helloMessage = Formatting.format template [| {Id=1; Name="Adam"} |]
// val helloMessage : string = "Hello, User { Id: 1, Name: "Adam" }"

(** Format the template using an F# DU, with a record argument?
 Notice the normally 'hidden' fields on the F# DU are still rendered
 (i.e. Tag, IsUser, etc).
 *)
Formatting.format template [| User({Id=99; Name="John"}) |]
// > val it : string =
//  "Hello, User { Item: User { Id: 99, Name: "John" }, Tag: 0, IsUser: True, IsManager: False }"

(** How can I capture and print F# Discriminated Unions better?
*destructuring* is fully customisable. TODO: maybe this common use
should be fixed in the API itself. *)
let myFsharpDestructurer : Destructurer =
    fun req -> TemplatePropertyValue.Empty
// Capturing.capturePropertiesCustom()

(** What if I have a better way to capture values? *)
let tw = new System.IO.StringWriter()
Formatting.formatCustom template tw (function
                                    | "person" -> SequenceValue [ScalarValue 1; ScalarValue 2]
                                    | "unused" -> ScalarValue (box 213.456)
                                    | _ -> TemplatePropertyValue.Empty)
// > val tw : System.IO.StringWriter = Hello, [1, 2]

(** How can I get the property names and values associated with the template? *)
Capturing.captureMessageProperties "{@easy} {@as}" [| 123; "abc" |] |> Seq.toArray
// val it : PropertyNameAndValue [] =
//  [|{Name = "easy"; Value = ScalarValue 123;}
//    {Name = "as"; Value = ScalarValue "abc";}|]

(** What happens when named property values aren't provied? *)
Formatting.format (Parser.parse "Hello, {$p1:l} and {$p2}") [| Manager({Name="Lumberg"; Reports=10}) |]
// > val it : string = "Hello, Lumberg and {$p2}"

(** What happens when *ALL* propertes are positional? *)
Formatting.format (Parser.parse "Hello, {1} and {0}") [| "b"; "a" |]
// > val it : string = "Hello, "a" and "b""

(** What happens when a positional parameter is missing? *)
Formatting.format (Parser.parse "Hello, {1} and {2}") [| "a"; "b" |]
// > val it : string = "Hello, "b" and {2}"

(** What happens when *NOT* ALL propertes are positional (but some are)? *)
Formatting.format (Parser.parse "Hello, {1} and {0} and {oops}") [| "b"; "a"; "c" |]
// Notice that all parameters were matched left-to-right, positional numbers ignored.
// > val it : string = "Hello, "b" and "a" and "c""

(** How does the parser work? *)
type TestItem = { Template: string
                  Values: System.Collections.IEnumerable // eeew
                  Expected: Token list
                  ExpectedString: string }

let testData = [
    { Template =        "test {test}";          Values = [123]
      ExpectedString =  "test 123"
      Expected =        [ Tk.text 0 "test "
                          Tk.prop 5 "{test}" "test" ] }

    { Template =        "test {test}{test}.";   Values = [123;456]
      ExpectedString =  "test 123456."
      Expected =        [ Tk.text 0 "test "
                          Tk.prop 5 "{test}" "test"
                          Tk.prop 11 "{test}" "test"
                          Tk.text 17 "." ] }

    { Template =        "test {@test};{$test};test";   Values = [543;567]
      ExpectedString =  "test 543;567;test"
      Expected =        [ Tk.text 0 "test "
                          Tk.propd 5 "{@test}" "test"
                          Tk.text 12 ";"
                          Tk.propds 13 "{$test}" "test"
                          Tk.text 20 ";test"] }

    { Template =        "test {0};{1};{2}";   Values = [box "a"; box 1; box "z"]
      ExpectedString =  "test a;1;z"
      Expected =        [ Tk.text 0 "test "
                          Tk.propp 5 0
                          Tk.text 8 ";"
                          Tk.propp 9 1
                          Tk.text 12 ";"
                          Tk.propp 13 2 ] }
]

testData
|> Seq.map (fun test -> test, Parser.parse test.Template)
|> Seq.map (fun (test, templ) -> Capturing.captureProperties templ (test.Values |> Seq.cast<obj> |> Seq.toArray))
|> Seq.toArray
// val it : seq<PropertyNameAndValue> [] =
//  [|[|{Name = "test"; Value = ScalarValue 123;}|];
//    [|{Name = "test"; Value = ScalarValue 123;}
//      {Name = "test"; Value = ScalarValue 456;}|];
//    [|{Name = "test"; Value = ScalarValue 543;}
//      {Name = "test"; Value = ScalarValue "567";}|];
//    [|{Name = "0"; Value = ScalarValue "a";}
//      {Name = "1"; Value = ScalarValue 1;};
//      {Name = "2"; Value = ScalarValue "z";}|]
// |]
