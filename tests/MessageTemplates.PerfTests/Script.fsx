#r "System.IO" // to be sure we get the PCL libraries

type Arguments = {
    ExportCharts: bool
    SaveOutput: bool
    RepeatCount: int
    RepeatStr: string
    ChartsFolder: string }

let getArgValueOrDefault name def =
    match System.Environment.GetEnvironmentVariable name with | null -> def | "" -> def | s -> s

let args = {
    ExportCharts = (getArgValueOrDefault "exportCharts" "true") = "true"
    ChartsFolder = getArgValueOrDefault "chartsFolder" "..\\..\\docs\\files\\img\\perfcharts\\"
    SaveOutput = (getArgValueOrDefault "saveOutput" "false") = "true"
    RepeatCount = int (getArgValueOrDefault "repeatCount" "10000")
    RepeatStr = getArgValueOrDefault "repeatStr" "10k x "
}

#I "../../packages/PerfUtil/lib/net40"
#r "PerfUtil"

#I "../../packages/Antlr4.StringTemplate/lib/net35"
#r "Antlr4.StringTemplate"

#I "../../packages/Serilog/lib/net45"
#r "Serilog"

#I "bin/Release/"
#r "FsMessageTemplates"
#r "MessageTemplates"

open PerfUtil

module FsFormat = FsMessageTemplates.Formatting
module FsParser = FsMessageTemplates.Parser
module FsCapture = FsMessageTemplates.Capturing
open MessageTemplates

module StringFormatComptible =
    type IParseFormatTest =
        inherit ITestable
        abstract ParseFormat : string -> obj[] -> unit
    
    let parseFormatTest name init run =
        let tw = new System.IO.StringWriter()
        { new IParseFormatTest with
            member __.Name = name
            member __.ParseFormat (template:string)
                                  (args:obj[]) = run tw template args
            member __.Fini () = if args.SaveOutput then
                                    System.IO.File.AppendAllText(name+" output.txt", tw.ToString())
            member __.Init () = init (tw) }

    let noInit (_) = ()

    let MtFs = parseFormatTest "MessageT F#" noInit (fun tw template args->
        FsFormat.fprintsm tw template args)

    let MtCs = parseFormatTest "MessageT C#" noInit (fun tw template args ->
        MessageTemplate.Format(tw.FormatProvider, tw, template, args))

    open Serilog
    let initSerilog (tw) =
        Serilog.Log.Logger <-
            Serilog.LoggerConfiguration()
                .MinimumLevel.Verbose()
                .WriteTo.TextWriter(tw)
                .CreateLogger()

    let Serilog = parseFormatTest "Serilog" initSerilog (fun tw template args ->
        Serilog.Log.Verbose(template, args))

    let TwCs = parseFormatTest "TextWriter" noInit (fun tw template args ->
        tw.Write(template, args))
    
    let positional = new ImplementationComparer<IParseFormatTest>(
                                        MtFs, [ TwCs; MtCs; Serilog; ],
                                        warmup=true, verbose=true)

    let namedAndDestruring = new ImplementationComparer<IParseFormatTest>(
                                    MtFs, [ MtCs; Serilog; ], warmup=true, verbose=true)

StringFormatComptible.positional.Run(
    id=args.RepeatStr + "3 posit. ints (AR5)", repeat=args.RepeatCount,
    testF=(fun o -> o.ParseFormat ("big long x{0,5:0,00}x{1,5:0,00}x{2,5:0,00}x") [|0;1;2|]))

StringFormatComptible.positional.Run(
    id=args.RepeatStr + "3 posit. ints fmt", repeat=args.RepeatCount,
    testF=(fun o -> o.ParseFormat ("big long x{0:0,00}x{1:0,00}x{2:0,00}x") [|0;1;2|]))

StringFormatComptible.positional.Run(
    id=args.RepeatStr + "5 posit. strings", repeat=args.RepeatCount,
    testF=(fun o -> o.ParseFormat ("big long {0}x{2}x{1}x{4}x{3}") [|"a";"b";"c";"d";"e"|]))

System.GC.Collect()
StringFormatComptible.namedAndDestruring.Run(
    id=args.RepeatStr + "3 named destr", repeat=args.RepeatCount,
    testF=(fun o -> o.ParseFormat "big long {@one} {@two} {@three}"
                                  [| System.Version(1, 1)
                                     (222, 222)
                                     System.ConsoleColor.Magenta |]))

System.GC.Collect()
StringFormatComptible.namedAndDestruring.Run(
    id=args.RepeatStr + "3 named destr (AL10)", repeat=args.RepeatCount,
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
        FsFormat.sprintsm invariant "Hello, {name}" args |> ignore )

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
                                            FsPrintf; StringWriter; (*Ant4St*) ],
                                    warmup=true, verbose=true)

AnyFormat.allBasicFormatters.Run(
    id="Format [|\"adam\"|]", repeat=args.RepeatCount,
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
                (fun () -> FsParser.parse("Release {$version} and {blah} and {blah2}"))
                (fun template -> FsFormat.format template args |> ignore)

    let MtCs = anyTemplateFormatTest
                "MessageT C#"
                (fun () -> MessageTemplate.Parse("Release {$version} and {blah} and {blah2}"))
                (fun template -> template.Format(invariant, args) |> ignore)

    let FsPrintf = anyTemplateFormatTest "F# sprintf" (fun () -> ()) (fun () ->
        Printf.sprintf "Release %O and %s and %s" theVersion "blah" "blah" |> ignore)

    open Serilog
    let initSerilog (tw) =
        Serilog.Log.Logger <-
            Serilog.LoggerConfiguration()
                .MinimumLevel.Verbose()
                .WriteTo.TextWriter(tw)
                .CreateLogger()

    let Serilog = anyTemplateFormatTest
                    "Serilog"
                    (fun () ->
                        initSerilog (new System.IO.StringWriter())
                        "Release {$version} and {blah} and {blah2}") // return the string as serilog caches it anyway
                    (fun (template) -> Serilog.Log.Verbose(template, args))

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
                                    MtFs, [ MtCs; Serilog; FsPrintf; StringFormat; (*Ant4St;*) ],
                                    warmup=true, verbose=true)

FormatTemplate.all.Run(
    id=args.RepeatStr + "format cached template", repeat=args.RepeatCount,
    testF=(fun t -> t.DoIt()))

module FormatStringify =
    let theVersion = System.Version(2, 2, 2, 2)
    let args = [| box theVersion |]

    let MtFs = anyFormatTest "MessageT F#" (fun () ->
        FsFormat.sprintsm invariant "Release {$version}" args |> ignore )

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
    id=args.RepeatStr + "Format stringify Version", repeat=args.RepeatCount,
    testF=fun t -> t.DoIt())

#load "../../packages/FSharp.Charting/FSharp.Charting.fsx"
open FSharp.Charting
open PerfUtil

if args.ExportCharts then
    // simple plot function
    let plot yaxis (metric : PerfResult -> float) (results : PerfResult list) =
        let values = results |> List.choose (fun r -> if r.HasFailed then None else Some (r.SessionId, metric r))
        let name = results |> List.tryPick (fun r -> Some r.TestId)
        let ch = Chart.Bar(values, ?Name = name, ?Title = name, YTitle = yaxis)
        let nameFixedForExport = name.Value.ToString().Replace("[|\"","_").Replace("\"|]", "_") + ".png"
        let exportFolder = System.IO.Path.Combine(__SOURCE_DIRECTORY__, args.ChartsFolder)
        System.IO.Directory.CreateDirectory(exportFolder) |> ignore
        let fileName = System.IO.Path.Combine(exportFolder, nameFixedForExport)
        System.Console.WriteLine("saving {0}", fileName)
        use f = ch.ShowChart()
        ch.SaveChartAs (fileName, ChartTypes.ChartImageFormat.Png)
        f.Close()

    // read performance tests from 'Tests' module and run them
    [
        AnyFormat.allBasicFormatters.GetTestResults()
        FormatStringify.allBasicFormatters.GetTestResults()
        FormatTemplate.all.GetTestResults()
        StringFormatComptible.namedAndDestruring.GetTestResults()
        StringFormatComptible.positional.GetTestResults()
    ]
    |> List.concat
    |> TestSession.groupByTest
    |> Map.iter (fun _ r -> plot "milliseconds" (fun t -> t.Elapsed.TotalMilliseconds) r)
