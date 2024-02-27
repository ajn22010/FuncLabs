// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO

let NounList = ["kaka"; "maka"; "iterator"; "hörlur"; "telefon"; "peng"; "träd"; "lista"; "spel"; "mat"]
let VerbList = ["kokar"; "bakar"; "rakar"; "knakar"; "läser"; "låser"; "rasar"; "hasar"; "sover"; "stänger"]

type Word =
    | Noun of String
    | Verb of String
    | Error of String  //Maybe use this to let program flow / deal with errors.

let wordToString x  =
    match x with
    | Noun(value) -> value
    | Verb(value) -> value

let swapTriple input = 
    match input with
    | (a,b,c) -> (b,a,c)
    | _ -> failwith "wat"

let getSentence input = 
    match input with
    | (a, b, c) -> wordToString(a) + " " + wordToString(b) + " " + wordToString(c)
    | _ -> failwith "incorrect triple"

let prepareSentence input = 
    match input with 
    |(Noun(a), Verb(b), Noun(c)) -> b.Remove(0,1).Insert(0, b.Chars(0).ToString().ToUpper()) + " " + a + " " + c + "?"
    |(Verb(a), Noun(b), Noun(c)) -> b.Remove(0,1).Insert(0, b.Chars(0).ToString().ToUpper()) + " " + a + " " + c + "."
    | _ -> "Ett fel uppstod."

let rec compareWord victim list = 
    match list with
    | [] -> false
    | head :: tail -> 
        if head = victim then
            true 
        else
            compareWord victim tail

let rec compareLists victim = 
    match victim with
    | [] -> true
    | head::[] -> (compareWord head NounList)  //Has to end in noun
    | head::tail -> ((compareWord head VerbList) || compareWord head NounList) && (compareLists tail)  
        
let rec sentenceBuilder inList = 
    match inList with
    | [] -> []
    | head::tail -> 
        if compareWord head NounList then
            Noun(head) :: sentenceBuilder tail
        else if compareWord head VerbList then
            Verb(head) :: sentenceBuilder tail
        else Error(head) :: sentenceBuilder tail

let printer input = printfn "%s" input

let validateString (validationObject : string list) = 
    if validationObject.Length <> 3 then   
        printfn "Invalid length."
        false
    else
    if compareLists validationObject = false then
        printfn "Invalid words or format."
        false
    else
        true

let rec stringGetter() = 
    printfn "Enter your string: "
    Console.ReadLine().ToLower().Split([|' '|])
    |> Array.toList

let makeTriple inputString = 
    match inputString with
    | a :: b :: c :: [] -> (a,b,c)
    | _ -> (Error("Error at makeTriple"),Error("Error at makeTriple"),Error("Error at makeTriple"))
        
let managingFunction inputString = 
    sentenceBuilder inputString
    |> makeTriple
    |> prepareSentence
    |> printer



printfn "Hello. Enter name of text file or \"input\" to enter your own sentences."

let option = Console.ReadLine()

match option with
    | "input" -> while true do
        let inputString = stringGetter()
        if not (validateString inputString) then
            printfn "Exiting..."
            exit(0)
        else
            managingFunction inputString
    | "exit" -> exit(0)
    | _ ->  let stream = 
                try 
                    new StreamReader(option)
                with
                | _ -> printf "Error opening file"; exit(0);
            while true do 
            let inputString =  
                try 
                    if not stream.EndOfStream then
                         stream.ReadLine().ToLower().Split([|' '|]) |> Array.toList
                    else 
                        printf "End of file"; exit(0);
                with
                | _ -> printf "End of file"; exit(0);

            if not (validateString inputString) then
            printfn "Exiting..."
            exit(0)
            else
                managingFunction inputString
