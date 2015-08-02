// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "System.Runtime"
#I "bin/Release/"
#r "FsMessageTemplates"
#r "MessageTemplates"

[<Literal>]
let template = "Hello, namasdf,asdfadam, how's it {going}?"
[<Literal>]
let iterations = 200000

System.GC.Collect(3, System.GCCollectionMode.Forced, blocking=true)
let p = System.Globalization.CultureInfo.InvariantCulture

#time "on"

printfn "F#"
for _ in 1..iterations do
    FsMessageTemplates.MessageTemplates.parse (template)
    // |> fun t -> FsMessageTemplates.MessageTemplates.format p t [|box "hanging"|]
    |> ignore

printfn "C#"
for _ in 1..iterations do
    MessageTemplates.MessageTemplate.Parse (template)
    // |> fun t -> t.Format(p, [|box "hanging"|])
    |> ignore

