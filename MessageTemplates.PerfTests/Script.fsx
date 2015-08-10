
#I "bin/Release/"
#r "FsMessageTemplates"
#r "MessageTemplates"

[<Literal>]
let template = "Hello, namasdf,asdfadam, how's it {going}?"
[<Literal>]
let iterations = 200000

let gc () = System.GC.Collect(3, System.GCCollectionMode.Forced, blocking=true)
let p = System.Globalization.CultureInfo.InvariantCulture

#time "on"
let sw = System.Diagnostics.Stopwatch()

gc ()
printfn "Starting F#..."
sw.Start()
for _ in 1..iterations do
    FsMessageTemplates.MessageTemplates.parse (template)
    |> fun t -> FsMessageTemplates.MessageTemplates.format p t [|box "hanging"|]
    |> ignore
printfn "F# completed in %A" sw.Elapsed

gc ()
printfn "Starting C#..."
sw.Restart()

for _ in 1..iterations do
    MessageTemplates.MessageTemplate.Parse (template)
    |> fun t -> t.Format(p, [|box "hanging"|])
    |> ignore
sw.Stop()
printfn "C# completed in %A" (sw.Elapsed.ToString())
