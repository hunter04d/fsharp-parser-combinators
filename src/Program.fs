// Learn more about F# at http://fsharp.org

open System
open EmailParser
open Parser
open System.Text



let printEmailTree list : unit = 
    let intent = "    "
    let emailToJsonLike (localPart, domainPart) = 
        let escape s:string = sprintf "\"%s\"" s
        let res = StringBuilder() 
        res
            .AppendLine("{")
            .AppendLine(sprintf "%s\"local part\": \"%s\"" intent localPart)
            .AppendLine(sprintf "%s\"domain part\":\n%s[" intent intent)
            .AppendJoin(",\n    ",  (List.map (escape >> (fun s -> intent + s)) domainPart) |> List.toSeq)
            .AppendLine("\n    ]")
            .Append("}")
            .ToString()   
    printfn "["
    printfn  "%s" (String.concat ",\n" (list |> List.map emailToJsonLike |> List.toSeq))
    printfn "]"


[<EntryPoint>]
let main argv =
    let res = 
        Parser.run emailList "e.email@email.com hello@p"
    match res with
    | Parser.Success (l, _) ->
        printEmailTree l
    | Parser.Failure _ -> Parser.printResult res
    Console.ReadKey(true) |> ignore
    0
