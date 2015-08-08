// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#I "bin/Debug/"
#r "FsMessageTemplates"

#load "../FsMessageTemplates.Tests/Tk.fs"

open FsMessageTemplates.MessageTemplates

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
|> Seq.map (fun test -> test, parse test.Template)
//|> Seq.map (fun (test, templ) -> captureProperties templ (test.Values |> Seq.cast<obj> |> Seq.toArray)) 
|> Seq.toArray
