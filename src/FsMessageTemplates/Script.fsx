
#I "bin/Debug/"
#r "FsMessageTemplates"

#load "../../tests/FsMessageTemplates.Tests/Tk.fs"

open FsMessageTemplates

let invariantCulture = System.Globalization.CultureInfo.InvariantCulture :> System.IFormatProvider
type User = { Id:int; Name:string }
type Manager = { Name:string; Reports:int }
type Person = User of User | Manager of Manager
    with override this.ToString() = match this with User u -> u.Name | Manager m -> m.Name

// Create a message template
let template = Parser.parse "Hello, {@person}"

Formatting.format template [| {Id=1; Name="Adam"} |]
// > val it : string = "Hello, User { Id: 1, Name: "Adam" }"

Formatting.format template [| User({Id=99; Name="John"}) |]
// Notice the normally 'hidden' fields on the F# DU are still rendered (i.e. Tag, IsUser, etc).
// > val it : string =
//  "Hello, User { Item: User { Id: 99, Name: "John" }, Tag: 0, IsUser: True, IsManager: False }"

(* What if I have a better way to capture values? *)
let tw = new System.IO.StringWriter()
Formatting.formatCustom template tw (function
                                    | "person" -> SequenceValue [ScalarValue 1; ScalarValue 2]
                                    | "unused" -> ScalarValue (box 213.456)
                                    | _ -> TemplatePropertyValue.Empty)
// > val tw : System.IO.StringWriter = Hello, [1, 2]

(* How can I get the property names and values associated with the template? *)
Capturing.captureMessageProperties "{@easy} {@as}" [| 123; "abc" |] |> Seq.toArray
// > val it : PropertyNameAndValue [] =
//  [|{Name = "easy"; Value = ScalarValue 123;}
//    {Name = "as"; Value = ScalarValue "abc";}|]

(* What happens when values aren't provied? *)
Formatting.format (Parser.parse "Hello, {$p1:l} and {$p2}") [| Manager({Name="Lumberg"; Reports=10}) |]
// > val it : string = "Hello, {$p1} and {$p2}"

(* What happens when *ALL* propertes are positional? *)
Formatting.format (Parser.parse "Hello, {1} and {2}") [| "a"; "b" |]
Formatting.format (Parser.parse "Hello, {1} and {0}") [| "b"; "a" |]
// > val it : string = "Hello, "b" and "a""

(* What happens when *NOT* ALL propertes are positional (but some are)? *)
Formatting.format (Parser.parse "Hello, {1} and {0} and {oops}") [| "b"; "a"; "c" |]
// Notice that all parameters were matched left-to-right, positional numbers ignored.
// > val it : string = "Hello, "b" and "a" and "c""


(* How does the parser work? *)
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
