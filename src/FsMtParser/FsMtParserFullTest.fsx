#load "FsMtParserFull.fs"

type Token = TextToken of string | PropToken of FsMtParserFull.Property
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
FsMtParserFull.parseParts "Hello {{@adam:blah}}, how are {{you}}?" foundText foundProp
tokens

for i = 0 to 1000000 do
    // tokens.Clear()
    FsMtParserFull.parseParts "Hello {adam:#0.000}, how are {you? you crazy invalid prop}" foundText foundProp
    // printfn "%A" (tokens.ToArray())
