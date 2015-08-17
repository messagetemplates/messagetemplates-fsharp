
#I "../packages/PerfUtil/lib/net40"
#r "PerfUtil"

#I "../packages/Antlr4.StringTemplate/lib/net35"
#r "Antlr4.StringTemplate"

#r "System.IO"
#I "bin/Release/"
#r "FsMessageTemplates"
#r "MessageTemplates"

open PerfUtil

open FsMessageTemplates.MessageTemplates
open MessageTemplates

module StringFormatComptible =
    type IParseFormatTest =
        inherit ITestable
        abstract ParseFormat : string -> obj[] -> unit
    
    let parseFormatTest name run =
        let tw = new System.IO.StringWriter()
        { new IParseFormatTest with
            member __.Name = name
            member __.ParseFormat (template:string)
                                  (args:obj[]) = run tw template args
            member __.Fini () = ()
            member __.Init () = () }

    let MtFs = parseFormatTest "MessageT F#" (fun tw template args->
        fprintsm tw template args)

    let MtCs = parseFormatTest "MessageT C#" (fun tw template args ->
        MessageTemplate.Format(tw.FormatProvider, tw, template, args))

    let TwCs = parseFormatTest "TextWriter" (fun tw template args ->
        tw.Write(template, args))
    
    let positional = new ImplementationComparer<IParseFormatTest>(
                                        MtFs, [ TwCs; MtCs; ],
                                        warmup=true, verbose=true)

    let namedAndDestruring = new ImplementationComparer<IParseFormatTest>(
                                    MtFs, [ MtCs; ], warmup=true, verbose=true)

StringFormatComptible.positional.Run(
    id="10k x 3 posit. ints (AR5)", repeat=10000,
    testF=(fun o -> o.ParseFormat ("big long x{0,5:0,00}x{1,5:0,00}x{2,5:0,00}x") [|0;1;2|]))

StringFormatComptible.positional.Run(
    id="10k x 3 posit. ints fmt", repeat=10000,
    testF=(fun o -> o.ParseFormat ("big long x{0:0,00}x{1:0,00}x{2:0,00}x") [|0;1;2|]))

StringFormatComptible.positional.Run(
    id="10k x 5 posit. strings", repeat=10000,
    testF=(fun o -> o.ParseFormat ("big long {0}x{2}x{1}x{4}x{3}") [|"a";"b";"c";"d";"e"|]))

System.GC.Collect()
StringFormatComptible.namedAndDestruring.Run(
    id="100k x 3 named destr", repeat=100000,
    testF=(fun o -> o.ParseFormat "big long {@one} {@two} {@three}"
                                  [| System.Version(1, 1)
                                     (222, 222)
                                     System.ConsoleColor.Magenta |]))

System.GC.Collect()
StringFormatComptible.namedAndDestruring.Run(
    id="100k x 3 named destr (AL10)", repeat=100000,
    testF=(fun o -> o.ParseFormat "{@one,-10} {@two,-10} {@three,-10}"
                                  [| System.Version(1, 1)
                                     (222, 222)
                                     System.ConsoleColor.Magenta |]))

type IFormatTest =
    inherit ITestable
    abstract DoIt : unit -> unit
    
let anyFormatTest name run =
    { new IFormatTest with
        member __.Name = name
        member __.DoIt () = run ()
        member __.Fini () = ()
        member __.Init () = () }

let invariant = System.Globalization.CultureInfo.InvariantCulture

module AnyFormat =
    let args = [| box "adam" |]

    let MtFs = anyFormatTest "MessageT F#" (fun () ->
        sprintsm invariant "Hello, {name}" args |> ignore )

    let MtCs = anyFormatTest "MessageT C#" (fun () ->
        MessageTemplate.Format(invariant, "Hello, {name}", args) |> ignore)

    let StringFormat = anyFormatTest "String.Format" (fun () ->
        System.String.Format(invariant, "Hello, {0}", args) |> ignore)

    let StringWriter = anyFormatTest "StringWriter" (fun () ->
        (new System.IO.StringWriter(invariant)).Write("Hello, {0}", args))

    let StringBuilder = anyFormatTest "StringBuilder" (fun () ->
        System.Text.StringBuilder().AppendFormat(invariant, "Hello, {0}", args) |> ignore)

    let FsPrintf = anyFormatTest "F# sprintf" (fun () ->
        Printf.sprintf "Hello, %s" "adam" |> ignore)

    let Ant4St = anyFormatTest "Antlr4.StringT" (fun () ->
        Antlr4.StringTemplate.Template("Hello, <name>")
            .Add("name", "adam")
            .Render(invariant) |> ignore)

    let allBasicFormatters = new ImplementationComparer<IFormatTest>(
                                    MtFs, [ MtCs; StringFormat; StringBuilder
                                            FsPrintf; StringWriter; Ant4St ],
                                    warmup=true, verbose=true)

AnyFormat.allBasicFormatters.Run(
    id="Format [|\"adam\"|]", repeat=100000,
    testF=fun t -> t.DoIt())

let anyTemplateFormatTest name createTemplate run =
    let template = createTemplate()
    { new IFormatTest with
        member __.Name = name
        member __.DoIt () = run (template)
        member __.Fini () = ()
        member __.Init () = () }

module FormatTemplate =
    let theVersion = System.Version(2, 2, 2, 2)
    let args = [| box theVersion; box "blah"; box "blah2" |]

    let MtFs = anyTemplateFormatTest
                "MessageT F#"
                (fun () -> parse("Release {$version} and {blah} and {blah2}"))
                (fun template -> format template args |> ignore)

    let MtCs = anyTemplateFormatTest
                "MessageT C#"
                (fun () -> MessageTemplate.Parse("Release {$version} and {blah} and {blah2}"))
                (fun template -> template.Format(invariant, args) |> ignore)

    let FsPrintf = anyTemplateFormatTest "F# sprintf" (fun () -> ()) (fun () ->
        Printf.sprintf "Release %O and %s and %s" theVersion "blah" "blah" |> ignore)

// TODO: is it just too slow or am I doing something wrong?
//    let Ant4St = anyTemplateFormatTest
//                    "Antlr4.StringT"
//                    (fun () -> Antlr4.StringTemplate.Template("Release <version>").CreateShadow())
//                    (fun template -> template |> ignore)

    let StringFormat = anyTemplateFormatTest
                        "String.Format"
                        (fun () -> ())
                        (fun () -> System.String.Format("Version {0} and {1} and {2}", args) |> ignore)

    let all = new ImplementationComparer<IFormatTest>(
                                    MtFs, [ MtCs; FsPrintf; StringFormat; (*Ant4St;*) ],
                                    warmup=true, verbose=true)

FormatTemplate.all.Run(
    id="100k x format cached template", repeat=100000,
    testF=(fun t -> t.DoIt()))

module FormatStringify =
    let theVersion = System.Version(2, 2, 2, 2)
    let args = [| box theVersion |]

    let MtFs = anyFormatTest "MessageT F#" (fun () ->
        sprintsm invariant "Release {$version}" args |> ignore )

    let MtCs = anyFormatTest "MessageT C#" (fun () ->
        MessageTemplate.Format(invariant, "Release {$version}", args) |> ignore)

    let StringFormat = anyFormatTest "String.Format" (fun () ->
        System.String.Format(invariant, "Release {0}", args) |> ignore)

    let StringWriter = anyFormatTest "StringWriter" (fun () ->
        (new System.IO.StringWriter(invariant)).Write("Release {0}", args))

    let StringBuilder = anyFormatTest "StringBuilder" (fun () ->
        System.Text.StringBuilder().AppendFormat(invariant, "Release {0}", args) |> ignore)

    let FsPrintf = anyFormatTest "F# sprintf" (fun () ->
        Printf.sprintf "Release %O" theVersion |> ignore)

    let allBasicFormatters = new ImplementationComparer<IFormatTest>(
                                    MtFs, [ MtCs; StringBuilder
                                            StringFormat; StringWriter
                                            FsPrintf ],
                                    warmup=true, verbose=true)

FormatStringify.allBasicFormatters.Run(
    id="Format stringify Version", repeat=100000,
    testF=fun t -> t.DoIt())

#load "../packages/FSharp.Charting/FSharp.Charting.fsx"

open FSharp.Charting
open PerfUtil

// simple plot function
let plot yaxis (metric : PerfResult -> float) (results : PerfResult list) =
    let values = results |> List.choose (fun r -> if r.HasFailed then None else Some (r.SessionId, metric r))
    let name = results |> List.tryPick (fun r -> Some r.TestId)
    let ch = Chart.Bar(values, ?Name = name, ?Title = name, YTitle = yaxis)
    let nameFixedForExport = name.Value.ToString().Replace("[|\"","_").Replace("\"|]", "_") + ".png"
    let exportFolder = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..\\artifacts\\perfcharts\\")
    System.IO.Directory.CreateDirectory(exportFolder) |> ignore
    let fileName = System.IO.Path.Combine(exportFolder, nameFixedForExport)
    System.Console.WriteLine("saving {0}", fileName)
    ch.ShowChart() |> ignore
    ch.SaveChartAs (fileName, ChartTypes.ChartImageFormat.Png)

// read performance tests from 'Tests' module and run them
let results =
    [ AnyFormat.allBasicFormatters.GetTestResults()
      FormatStringify.allBasicFormatters.GetTestResults()
      FormatTemplate.all.GetTestResults()
      StringFormatComptible.namedAndDestruring.GetTestResults()
      StringFormatComptible.positional.GetTestResults()
    ]
    |> List.concat
    |> TestSession.groupByTest
    |> Map.iter (fun _ r -> plot "milliseconds" (fun t -> t.Elapsed.TotalMilliseconds) r)
