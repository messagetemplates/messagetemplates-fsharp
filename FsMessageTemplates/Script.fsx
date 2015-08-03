// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "System.Runtime"
#I "bin/Debug/"
#r "FsMessageTemplates"
#I "../packages/xunit.assert.2.0.0/lib/portable-net45+win+wpa81+wp80+monotouch+monoandroid+Xamarin.iOS/"
#r "xunit.assert"

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

open System
type DtOffset = System.DateTimeOffset

let builtInScalarTypeFactories : (System.Type * (obj->Scalar option)) array =
    [|
         typedefof<bool>,       function :? bool as v -> Some (Scalar.Bool v)               | _ -> None
         typedefof<char>,       function :? char as v -> Some (Scalar.Char v)               | _ -> None
         typedefof<byte>,       function :? byte as v -> Some (Scalar.Byte v)               | _ -> None
         typedefof<int16>,      function :? int16 as v -> Some (Scalar.Int16 v)             | _ -> None
         typedefof<uint16>,     function :? uint16 as v -> Some (Scalar.UInt16 v)           | _ -> None
         typedefof<int32>,      function :? int as v -> Some (Scalar.Int32 v)               | _ -> None
         typedefof<uint32>,     function :? uint32 as v -> Some (Scalar.UInt32 v)           | _ -> None
         typedefof<int64>,      function :? int64 as v -> Some (Scalar.Int64 v)             | _ -> None
         typedefof<uint64>,     function :? uint64 as v -> Some (Scalar.UInt64 v)           | _ -> None
         typedefof<single>,     function :? single as v -> Some (Scalar.Single v)           | _ -> None
         typedefof<double>,     function :? double as v -> Some (Scalar.Double v)           | _ -> None
         typedefof<decimal>,    function :? decimal as v -> Some (Scalar.Decimal v)         | _ -> None
         typedefof<string>,     function :? string as v -> Some (Scalar.String v)           | _ -> None
         typedefof<DateTime>,   function :? DateTime as v -> Some (Scalar.DateTime v)       | _ -> None
         typedefof<DtOffset>,   function :? DtOffset as v -> Some (Scalar.DateTimeOffset v) | _ -> None
         typedefof<TimeSpan>,   function :? TimeSpan as v -> Some (Scalar.TimeSpan v)       | _ -> None
         typedefof<Guid>,       function :? Guid as v -> Some (Scalar.Guid v)               | _ -> None
         typedefof<Uri>,        function :? Uri as v -> Some (Scalar.Uri v)                 | _ -> None
    |]

let builtInScalarDictionary = builtInScalarTypeFactories |> dict
let builtInTryConvertToScalar value =
    if value = null then None
    else match builtInScalarDictionary.TryGetValue (value.GetType()) with
         | false, _ -> None
         | true, tryGetFn ->
            match tryGetFn value with | Some v -> Some (ScalarValue v) | _ -> None

let rec destructure (tryGetScalar: obj -> TemplatePropertyValue option)
                    (tryIfDestructure: obj -> TemplatePropertyValue option)
                    (tryIfEnumerable: System.Collections.IEnumerable -> TemplatePropertyValue option)
                    (getChildProperties: obj -> (string * obj) seq)
                    (destr: DestructureKind)
                    (value: obj) =

    let recChildDestrc = destructure tryGetScalar
                                     tryIfDestructure
                                     tryIfEnumerable
                                     getChildProperties
                                     DestructureKind.Destructure

    let typeOrNone = function null -> None | v -> Some (v.GetType())
    match value, typeOrNone value, destr with
    | null, _, _                                -> Some (ScalarValue <| Scalar.Null)
    | value, Some _, DestructureKind.Stringify  -> Some (ScalarValue <| (Scalar.String <| value.ToString()))
    | value, Some ty, destr -> 
        let valAsEnumerable = match value with :? Collections.IEnumerable as en -> Some en | _ -> None
        if destr = DestructureKind.Destructure then
            match tryIfDestructure value with
            | Some v -> Some v
            | _ -> None // TODO
        elif valAsEnumerable.IsSome then
            match tryIfEnumerable valAsEnumerable.Value with
            | Some v -> Some v
            | _ -> None // TODO:
        elif destr = DestructureKind.Destructure then
            let typeTag = if ty.Name = "" || not (Char.IsLetter ty.Name.[0]) then None else Some ty.Name
            let childValues = getChildProperties value
                              |> Seq.map (fun (name, childValue) -> name, (recChildDestrc childValue))
                              |> Seq.choose (function (name, Some value) -> Some (name, value) | (_, _) -> None)
                              |> Seq.toList
            Some (StructureValue (typeTag, childValues))
        else
            // No further options, convert to a string
            Some (ScalarValue <| Scalar.String (value.ToString()))
    | _, _, _ -> None

let destrDelgate : Destructurer = match o with :? Delegate as del -> Some <| ScalarValue (Scalar.String (string del)) | _ -> None
let destrNullable (destr:Destructurer) (o:obj) =
    let inner = match o with :? Nullable<_> as n when n.HasValue -> Some n.Value | _ -> None
    inner |> Option.map (fun v -> v) |> 

let builtInTryIfDestructure 
let defaultDestr = destructure builtInTryConvertToScalar
                               builtInTryIfDestructure
                               builtInTryIfEnumerable
                               builtInGetChildProperties

(*
let castToArray = Seq.cast<obj> >> Seq.toArray
let formatEnumerable template enumerable =
    format System.Globalization.CultureInfo.InvariantCulture
           template
           (castToArray enumerable)

let runParseTest data =
    let template = parse data.Template
    printfn "PARSE TEST\n Expected = %s\n Actual   = %s" data.Template template.FormatString 
    Xunit.Assert.Equal(data.Template, template.FormatString)
    let expectedArray = data.Expected |> List.toArray
    let actualArray = template.Tokens |> List.toArray
    Xunit.Assert.Equal<Token>(expectedArray, actualArray)

let runFormatTest data =
    let template = parse data.Template
    let actual = formatEnumerable template data.Values
    printfn "FMT TEST\n Expected = %s\n Actual   = %s" data.Template actual 
    Xunit.Assert.Equal(data.ExpectedString, actual)

testData |> List.iter runParseTest
testData |> List.iter runFormatTest
*)



