
/// Describes the string template parts in an fairly generic way
type TestTemplateToken =
    | Literal of raw:string
    | NamedProperty of name:string * value:obj

type TestTemplate =
    { Title: string
      TokenValues: TestTemplateToken list
      DictionaryOfNamesAndValues: System.Collections.Generic.IDictionary<string, obj>
      ObjectWithNamedProps: obj }

module TestInputs =
    type Priority = { p1: string }
    let t1 = { Title = "single string arg"
               TokenValues = [ Literal "you have priority "; NamedProperty("p1", "one"); Literal "\n" ]
               DictionaryOfNamesAndValues = ["p1", box "one"] |> dict
               ObjectWithNamedProps = { p1="one" } }

    type Person = { name: string; manager: string }
    let t2 = { Title = "two string args"
               TokenValues = [ Literal "hello, "; NamedProperty("name", "adam")
                               Literal ", your manager is "; NamedProperty("manager", "adam")
                               Literal "\n" ]
               DictionaryOfNamesAndValues = ["name", box "adam"; "manager", box "adam"] |> dict
               ObjectWithNamedProps = { name="adam"; manager="adam" } }

module Features =   
    /// Allows a tested implementation to configure it's output with the provided TextWriter
    type OutputInitialiser<'OutputSink> = System.IO.TextWriter -> 'OutputSink

    /// Takes our common test template as input and produces a template usable by the tested implementation.
    type ParseInputCreator<'ImplementationParseInput> = TestTemplate -> 'ImplementationParseInput

    /// Takes our common test template as input and produces arguments that can be formatted by the
    /// tested implementation.
    type TemplateArgsCreator<'ImplementationArgs> = TestTemplate -> 'ImplementationArgs

    /// Accepts the template and arguments/values as input, then writes the formatted output. All tested
    /// implementations must be capable of this feature.
    type ParseCaptureFormat<'ImplementationParseInput, 'FormatArgs, 'OutputSink> =
        'ImplementationParseInput -> 'FormatArgs -> 'OutputSink -> unit

    /// Accepts the template as input and produces a parsed or pre-processed version of it. Not all tested implementations
    /// are capable of this feature.
    type TemplateParser<'ImplementationParseInput, 'ParsedTemplate> = 'ImplementationParseInput -> 'ParsedTemplate

    /// Takes a parsed template and values and produces an implementation-specific structure that contains
    /// the captured properties. Not all tested implementations are capable of this feature.
    type Capturer<'ParsedTemplate, 'FormatArgs, 'CapturedProperties> =
        'ParsedTemplate -> 'FormatArgs -> 'CapturedProperties

    /// Takes a parsed template and values then formats and renders the output. Not all tested implementations are
    /// capable of this feature.
    type CaptureFormatter<'ParsedTemplate, 'FormatArgs, 'OutputSink> =
        'ParsedTemplate -> 'FormatArgs -> 'OutputSink -> unit

    /// Takes a parsed template and pre-captured values and renders them to the output. Not all tested implementations
    /// are capable of this feature.
    type Formatter<'ParsedTemplate, 'CapturedProperties, 'OutputSink> =
        'ParsedTemplate -> 'CapturedProperties -> 'OutputSink -> unit

#r "System.IO"
#I "bin/Release/"
#r "FsMessageTemplates"
#r "MessageTemplates"
#r "MessageTemplates.PerfTests"

#I "../packages/PerfUtil/lib/net40"
#r "PerfUtil"

#I "../packages/Antlr4.StringTemplate/lib/net35"
#r "Antlr4.StringTemplate"

#I "../packages/Serilog/lib/net45"
#r "Serilog"
#r "Serilog.FullNetFx"

#I "../packages/NamedFormat/lib"
#r "NamedFormat"

open PerfUtil

module Implementations =
    open System.Collections.Generic

    /// Describes an implementation of the various features we are testing. All testables must
    /// have at least a name, and be capable of implementing the ParseCaptureFormat<'OutputSink>'
    /// style which takes a string template and the values as input, then writes the formatted
    /// output to the text writer.
    type Implementation<'ParseInput, 'FormatArgs, 'OutputSink, 'ParsedTemplate, 'CapturedProperties> =
        { Name:                 string
          /// Allows the testable implementation to create and configure some kind of object that
          /// that will use the provided TextWriter as output.
          initOutput:           Features.OutputInitialiser<'OutputSink>
          /// Allows the testable implementation to convert the template into a format suitable
          /// for the implementation to use.
          initTemplate:         Features.ParseInputCreator<'ParseInput>
          initTemplateArgs:     Features.TemplateArgsCreator<'FormatArgs>

          /// Required. Takes the parsed template as input, the 
          parseCaptureFormat:   Features.ParseCaptureFormat<'ParseInput, 'FormatArgs, 'OutputSink>

          parse:                Features.TemplateParser<'ParseInput, 'ParsedTemplate> option
          capture:              Features.Capturer<'ParsedTemplate, 'FormatArgs, 'CapturedProperties> option
          captureFormat:        Features.CaptureFormatter<'ParsedTemplate, 'FormatArgs, 'OutputSink> option
          format:               Features.Formatter<'ParsedTemplate, 'CapturedProperties, 'OutputSink> option }

    let private initUsingTextWriter : Features.OutputInitialiser<System.IO.TextWriter> = fun tw -> tw

    /// Creates a named format template string such as "Hello, {name}" from the common test templates.
    let private namedFormatStringTemplateCreator : Features.ParseInputCreator<string> =
        fun tt -> tt.TokenValues |> List.map (function Literal raw -> raw | NamedProperty (n,_) -> "{" + n + "}") |> String.concat ""

    /// Creates a positional format template string such as "Hello, {0}" from the common test templates.
    let private positionalFormatStringTemplateCreator : Features.ParseInputCreator<string> =
        fun tt ->
            let mutable pos = 0
            let nextPos() = let beforeIncr = pos in pos<-pos+1; beforeIncr
            tt.TokenValues
            |> List.map (function
                | Literal raw -> raw
                | NamedProperty (n,_) -> "{" + string (nextPos()) + "}")
            |> String.concat ""

    /// Given a TestTemplate as input, this creates an obj[] of property values for input to
    /// the format implementations.
    let private positionalFormatArgsArrayCreator : Features.TemplateArgsCreator<obj[]> =
        fun tt -> tt.TokenValues |> List.choose (function NamedProperty (_,v) -> Some v | _ -> None) |> Array.ofList

    let private fsPickValueByName (pnvs: FsMessageTemplates.PropertyNameAndValue seq) name =
        pnvs |> Seq.pick (fun pnv -> if pnv.Name = name then Some (pnv.Value) else None)

    let fsMessageTemplates = {
        Name                = "F# MessageTemplates"
        initOutput          = initUsingTextWriter
        initTemplate        = namedFormatStringTemplateCreator
        initTemplateArgs    = positionalFormatArgsArrayCreator
        parseCaptureFormat  = fun mt vals tw -> FsMessageTemplates.Formatting.fprintsm tw mt vals
        parse               = Some <| fun mt -> FsMessageTemplates.Parser.parse mt
        capture             = Some <| fun tm args -> FsMessageTemplates.Capturing.captureProperties tm args
        captureFormat       = Some <| fun tm args tw -> FsMessageTemplates.Formatting.fprintm tm tw args
        format              = Some <| fun tm pnvs tw -> FsMessageTemplates.Formatting.formatCustom tm tw (fsPickValueByName pnvs) }


    open System.Linq // For enumerable.ToDictionary()

    let csMessageTemplates = {
        Name                = "C# MessageTemplates"
        initOutput          = initUsingTextWriter
        initTemplate        = namedFormatStringTemplateCreator
        initTemplateArgs    = positionalFormatArgsArrayCreator
        parseCaptureFormat  = fun mt vals tw -> MessageTemplates.MessageTemplate.Format(tw.FormatProvider, tw, mt, vals)
        parse               = Some <| fun mt -> MessageTemplates.MessageTemplate.Parse(mt)
        capture             = Some <| fun tm args -> MessageTemplates.MessageTemplate.Capture(tm, args)
        captureFormat       = Some <| fun tm args tw -> tm.Format(tw.FormatProvider, tw, args)
        format              = Some <| fun tm tps tw ->
                                        // TODO: maybe implement IReadOnlyDictionary<TKey, TValue> over array?
                                        let dict = tps.ToDictionary((fun tp -> tp.Name),
                                                                    elementSelector=(fun tp -> tp.Value))
                                        tm.Render(dict, tw, tw.FormatProvider) }

    open Serilog

    let serilog = {
        Name                = "Serilog"
        initOutput          = fun tw -> Serilog.LoggerConfiguration().WriteTo.TextWriter(tw, outputTemplate="{Message}").CreateLogger() 
        initTemplate        = namedFormatStringTemplateCreator
        initTemplateArgs    = positionalFormatArgsArrayCreator
        parseCaptureFormat  = fun mt vals logger -> logger.Information(mt, vals)
        parse               = Some <| fun mt -> Serilog.Parsing.MessageTemplateParser().Parse(mt)
        capture             = Some <| fun tm args ->
                                        // TOOD: is this the most similar to to capture values ??
                                        let mutable logger : Serilog.ILogger = null
                                        for i = 0 to args.Length - 1 do
                                            logger <- Serilog.Log.Logger.ForContext(string i, args.[i], destructureObjects=true)    
                                        logger
        captureFormat       = Some <| fun tm args logger -> logger.Information(tm.Text, args)
        format              = Some <| fun tm withCaptured _ -> withCaptured.Information(tm.Text)
    }

    let ioTextWriter = {
        Name                = "TextWriter"
        initOutput          = initUsingTextWriter
        initTemplate        = positionalFormatStringTemplateCreator
        initTemplateArgs    = positionalFormatArgsArrayCreator
        parseCaptureFormat  = fun mt vals tw -> tw.Write(mt, vals)
        parse               = None // no way to pre-parse this
        capture             = None // no way to parse or capture property names
        captureFormat       = None // no way to parse or capture property names
        format              = None // no way to parse or capture property names
    }

    let ioStringBuilder = {
        Name                = "StringBuilder"
        initOutput          = fun tw -> new System.Text.StringBuilder()
        initTemplate        = positionalFormatStringTemplateCreator
        initTemplateArgs    = positionalFormatArgsArrayCreator
        parseCaptureFormat  = fun mt vals sb -> sb.AppendFormat(mt, vals) |> ignore
        parse               = None // no way to pre-parse this
        capture             = None // no way to parse or capture property names
        captureFormat       = None // no way to parse or capture property names
        format              = None // no way to parse or capture property names
    }
    
    let private nameValueHashtableArgsCreator : Features.TemplateArgsCreator<System.Collections.Hashtable> =
        fun tt ->
            let ht = System.Collections.Hashtable()
            tt.TokenValues
            |> List.choose (function NamedProperty (n,v) -> Some (n, v) | _ -> None)
            |> List.iter (fun (n,v) -> ht.Add(n, v))
            ht

    let stringInject = {
        Name                = "StringInject"
        initOutput          = initUsingTextWriter
        initTemplate        = namedFormatStringTemplateCreator
        initTemplateArgs    = nameValueHashtableArgsCreator
        parseCaptureFormat  = fun mt vals tw -> tw.Write(StringInject.StringInjectExtension.Inject(mt, vals))
        parse               = None // no way to pre-parse this
        capture             = None // no way to parse or capture property names
        captureFormat       = None // no way to parse or capture property names
        format              = None // no way to parse or capture property names
    }

    open NamedFormat

    let private createNamedImpl name format : Implementation<string, obj, System.IO.TextWriter, unit, unit> =
        { Name                = name
          initOutput          = initUsingTextWriter
          initTemplate        = namedFormatStringTemplateCreator
          initTemplateArgs    = fun tt -> tt.ObjectWithNamedProps
          parseCaptureFormat  = fun mt args tw -> tw.Write(format=format mt args)
          parse               = None // no way to pre-parse this
          capture             = None // no way to parse or capture property names
          captureFormat       = None // no way to parse or capture property names
          format              = None // no way to parse or capture property names
        }

    let haackFormat     = createNamedImpl "HaackFormat"     (fun mt args -> mt.HaackFormat(args))
    let hanselmanFormat = createNamedImpl "HanselmanFormat" (fun mt args -> mt.HanselmanFormat(args))
    let henriFormat     = createNamedImpl "HenriFormat"     (fun mt args -> mt.HenriFormat(args))
    let jamesFormat     = createNamedImpl "JamesFormat"     (fun mt args -> mt.JamesFormat(args))
    let oskarFormat     = createNamedImpl "OskarFormat"     (fun mt args -> mt.OskarFormat(args))

    // Hey this is getting too tricky, we don't know the type structure of the templates dynamically
//    let objToPrintfSpecifier = function
//        | Literal raw -> raw
//        | NamedProperty (_, v) ->
//            match v with
//            | :? string  -> "%s"
//            | :? int -> "%i"
//            | :? bool -> "%b"
//            | :? char -> "%c"
//            | :? decimal -> "%M"
//            | _ -> "%O"
//    
//    let fsprintf = {
//        Name                = "F# fprintf"
//        initOutput          = initUsingTextWriter
//        initTemplate        = fun tt ->
//                                let templateString = tt |> Seq.map objToPrintfSpecifier |> String.concat ""
//                                Printf.TextWriterFormat<unit>(templateString)
//        initTemplateArgs    = fun tt ->
//                                let args = fun p1 -> ()
//                                args
//        parseCaptureFormat  = fun mt vals tw -> Printf.fprintf tw mt (obj())
//        parse               = Some <| fun mt -> Printf.TextWriterFormat<obj[]>(mt)
//        capture             = None // no way to parse or capture property names
//        captureFormat       = None // no way to parse or capture property names
//        format              = None // no way to parse or capture property names
//    }

type IFormatTest =
    inherit ITestable
    abstract DoIt : unit -> unit
    
let createParseCaptureFormatTest testTemplate tw (impl: Implementations.Implementation<_,_,_,_,_>) =
    let template = impl.initTemplate testTemplate
    let args = impl.initTemplateArgs testTemplate
    let state = impl.initOutput tw
    { new IFormatTest with
        member __.Name = impl.Name
        member __.DoIt () = impl.parseCaptureFormat template args state
        member __.Fini () = ()
        member __.Init () = () }

let createCaptureFormatTest testTemplate tw (impl: Implementations.Implementation<_,_,_,_,_>) =
    let template = impl.parse.Value (impl.initTemplate testTemplate)
    let args = impl.initTemplateArgs testTemplate
    let state = impl.initOutput tw
    { new IFormatTest with
        member __.Name = impl.Name
        member __.DoIt () = impl.captureFormat.Value template args state
        member __.Fini () = ()
        member __.Init () = () }

open Implementations
let createComparer tt =
    let tw = new System.IO.StringWriter() :> System.IO.TextWriter
    let theOne = createCaptureFormatTest tt tw fsMessageTemplates
    let theOthers = [
        createCaptureFormatTest tt tw csMessageTemplates
        createParseCaptureFormatTest tt tw serilog
        // createNamedFormatTest tt tw ioStringBuilder
        // createNamedFormatTest tt tw ioTextWriter
        createParseCaptureFormatTest tt tw stringInject
        createParseCaptureFormatTest tt tw haackFormat
        createParseCaptureFormatTest tt tw hanselmanFormat
        createParseCaptureFormatTest tt tw henriFormat
        createParseCaptureFormatTest tt tw jamesFormat
        createParseCaptureFormatTest tt tw oskarFormat
    ]
    ImplementationComparer(theOne, theOthers, warmup=true, verbose=true, throwOnError=false)

let comparers = [
    TestInputs.t1, createComparer TestInputs.t1
    TestInputs.t2, createComparer TestInputs.t2 ]

comparers |> List.iter (fun (tt, c) -> c.Run(id="(cached) 100k x " + tt.Title, repeat=100000, testF=fun t -> t.DoIt()))

#load "../packages/FSharp.Charting/FSharp.Charting.fsx"
open FSharp.Charting
open PerfUtil

// simple plot function
let plot yaxis (metric : PerfResult -> float) (results : PerfResult list) =
    let values = results |> List.choose (fun r -> if r.HasFailed then None else Some (r.SessionId, metric r))
    let name = results |> List.tryPick (fun r -> Some r.TestId)
    let ch = Chart.Bar(values, ?Name = name, ?Title = name, YTitle = yaxis)
    ch.ShowChart() |> ignore

    let nameFixedForExport = name.Value.ToString().Replace("[|\"","_").Replace("\"|]", "_") + ".png"
    let exportFolder = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..\\artifacts\\perfcharts\\")
    System.IO.Directory.CreateDirectory(exportFolder) |> ignore
    let fileName = System.IO.Path.Combine(exportFolder, nameFixedForExport)
    System.Console.WriteLine("saving {0}", fileName)
    ch.SaveChartAs (fileName, ChartTypes.ChartImageFormat.Png)

// read performance tests from 'Tests' module and run them
comparers
|> List.map (fun (tt, c) -> c.GetTestResults())
|> List.concat
|> TestSession.groupByTest
|> Map.iter (fun _ r -> plot "milliseconds" (fun t -> t.Elapsed.TotalMilliseconds) r)
