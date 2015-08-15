
#r "System.IO"
#I "bin/Release/"
#r "FsMessageTemplates"
#r "MessageTemplates"

[<Literal>]
let template = "Hello, {@name}, how's it {@going}?"
let chair = System.Version(1,2,3,4)
let args = [| box "adam"; box chair |]
[<Literal>]
let iterations = 200000

fsi.PrintDepth = 1
module t = let tw = new System.IO.StringWriter()

let gc () = System.GC.Collect(3, System.GCCollectionMode.Forced, blocking=true)
let p = System.Globalization.CultureInfo.InvariantCulture

#time "on"
let sw = System.Diagnostics.Stopwatch()

gc ()
printfn "Starting F#..."
sw.Start()

for i = 1 to iterations do
    FsMessageTemplates.MessageTemplates.fprintsm t.tw template args
    t.tw.WriteLine()

printfn "F# completed in %A" sw.Elapsed

gc ()
printfn "Starting C#..."
sw.Restart()

for i = 1 to iterations do
    MessageTemplates.MessageTemplate.Format(p, t.tw, template, args)
    t.tw.WriteLine()

sw.Stop()
printfn "C# completed in %A" (sw.Elapsed.ToString())
