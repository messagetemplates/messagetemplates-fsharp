// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#I "bin/Debug/"
#r "FsMessageTemplates"
#I "../packages/xunit.assert.2.0.0/lib/portable-net45+win+wpa81+wp80+monotouch+monoandroid+Xamarin.iOS/"
#r "xunit.assert"

open FsMessageTemplates.MessageTemplates

type TestItem = { Template: string
                  Values: System.Collections.IEnumerable
                  Expected: Token list
                  ExpectedString: string }

let emptyProp = { Name=""; Index=0; Align=None; DestructuringHint=None; Format=None; }

/// Named property token with an index
let np name index =
    PropertyToken({ emptyProp with
                        Name = name
                        Index = index })

/// Destructured Named property token with an index
let dnp name index =
    PropertyToken({ emptyProp with
                        Name = name
                        DestructuringHint = Some Destructure.Destructure
                        Index = index })

/// Stringify Named property token with an index
let snp name index =
    PropertyToken({ emptyProp with
                        Name = name
                        DestructuringHint = Some Destructure.Stringify
                        Index = index })

/// Positional property token with an index
let pp (position:int) index =
    PropertyToken({ emptyProp with
                        Name = sprintf "%i" position
                        Index = index })

/// Text token with an index
let tx text index = TextToken({ Text = text; Index = index })

let testData = [
    { Template =        "test {test}";          Values = [123]
      ExpectedString =  "test 123"
      Expected =        [ tx "test " 0
                          np "test" 5 ] }

    { Template =        "test {test}{test}.";   Values = [123;456]
      ExpectedString =  "test 123456."
      Expected =        [ tx "test " 0
                          np "test" 5
                          np "test" 11
                          tx "." 17 ] }

    { Template =        "test {@test};{$test};test";   Values = [543;567]
      ExpectedString =  "test 543;567;test"
      Expected =        [ tx "test " 0
                          dnp "test" 5; tx ";" 12
                          snp "test" 13; tx ";test" 20 ] }

    { Template =        "test {0};{1};{2}";   Values = [box "a"; box 1; box "z"]
      ExpectedString =  "test a;1;z"
      Expected =        [ tx "test " 0
                          pp 0 5; tx ";" 8
                          pp 1 9; tx ";" 12
                          pp 2 13 ] }
]

let castToArray = Seq.cast<obj> >> Seq.toArray
let formatEnumerable template enumerable =
    format System.Globalization.CultureInfo.InvariantCulture
           template
           (castToArray enumerable)

let runParseTest data =
    let template = parse data.Template
    printfn "TEST\n Expected = %s\n Actual   = %s" data.Template template.FormatString 
    Xunit.Assert.Equal(data.Template, template.FormatString)
//    printfn "Expected=%A" data.Expected
//    printfn "Actual=%A" template.Tokens
    Xunit.Assert.Equal<Token list>(data.Expected, template.Tokens)

let runFormatTest data =
    let template = parse data.Template
    let actual = formatEnumerable template data.Values
    Xunit.Assert.Equal(data.ExpectedString, actual)

testData |> List.iter runParseTest
testData |> List.iter runFormatTest



