#load "FsMtParser.fs"

type Token = TextToken of string | PropToken of FsMtParser.Property
let tokens = ResizeArray<Token>()

let foundText text =
    tokens.Add(TextToken(text))
    () // printfn "TEXT: %s"
let foundProp prop =
    tokens.Add(PropToken(prop))
    () //printfn "PROP: %A" prop
//FsMtLite.parseParts "Hello {@adam:blah}, how are {you?" foundText foundProp

#time "on"
;;

for i = 0 to 1000000 do
    // tokens.Clear()
    FsMtParser.parseParts "Hello {adam:#0.000}, how are {you? you crazy invalid prop}" foundText foundProp
    // printfn "%A" (tokens.ToArray())

