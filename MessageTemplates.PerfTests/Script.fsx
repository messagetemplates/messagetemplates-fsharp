
#I "../packages/PerfUtil/lib/net40"
#r "PerfUtil"

#I "../packages/suave/lib/net40"
#r "Suave"

#r "System.IO"
#I "bin/Release/"
#r "FsMessageTemplates"
#r "MessageTemplates"

open PerfUtil

open FsMessageTemplates.MessageTemplates
open MessageTemplates

module ParseFormat =
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

    let TwCs = parseFormatTest "TextWriter C#" (fun tw template args ->
        tw.Write(template, args))

let mtCsFsAndTextWriterTests = new ImplementationComparer<ParseFormat.IParseFormatTest>(
                                ParseFormat.MtFs, [ ParseFormat.MtCs; ParseFormat.TwCs ])

mtCsFsAndTextWriterTests.Run(id="10k x 3 pos args, all ints", repeat=10000,
    testF=(fun o -> o.ParseFormat ("{0} {1} {2}") [|0;1;2|]))

mtCsFsAndTextWriterTests.Run(id="10k x 5 pos args, all strings", repeat=10000,
    testF=(fun o -> o.ParseFormat ("{0}{2}{1}{4}") [|0;1;2;3;4|]))


let namedTests = new ImplementationComparer<ParseFormat.IParseFormatTest>(
                                ParseFormat.MtFs, [ ParseFormat.MtCs ])

System.GC.Collect()
namedTests.Run(id="100k x named destr", repeat=100000,
    testF=(fun o -> o.ParseFormat "{@one} {@two} {@three}"
                                  [| System.Version(1, 1)
                                     (222, 222)
                                     System.ConsoleColor.Magenta |]))

System.GC.Collect()
namedTests.Run(id="100k x named destr (AL10)", repeat=100000,
    testF=(fun o -> o.ParseFormat "{@one,-10} {@two,-10} {@three,-10}"
                                  [| System.Version(1, 1)
                                     (222, 222)
                                     System.ConsoleColor.Magenta |]))
