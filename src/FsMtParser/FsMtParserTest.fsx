#load "FsMtParser.fs"

type Token = TextToken of string | PropToken of FsMtParser.Property
let tokens = ResizeArray<Token>()

let foundText text =
    tokens.Add(TextToken(text))
    () // printfn "TEXT: %s"
let foundProp prop =
    tokens.Add(PropToken(prop))
    () //printfn "PROP: %A" prop

#time "on"
;;

tokens.Clear()
FsMtParser.parseParts "This {{adam:blah}}, is all text {{you}}?" foundText foundProp
FsMtParser.parseParts "This {{@adam:blah}}, is all text {{you}}?" foundText foundProp
FsMtParser.parseParts "This {adam:blah}, has actual valid properties?" foundText foundProp
tokens

for i = 0 to 1000000 do
    // tokens.Clear()
    FsMtParser.parseParts "Hello {adam:#0.000}, how are {you? you crazy invalid prop}" foundText foundProp
    // printfn "%A" (tokens.ToArray())
