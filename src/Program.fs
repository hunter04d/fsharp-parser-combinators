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
            .Append(sprintf "%s\"domain part\":\n%s[\n%s" intent intent intent)
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
        runOnString emailList @"
                            from'olegs'select'oleg{'where'oleg.LastName==Dubinskiy}@orevo.this-is-a-valid-email
                            for'parser'in'parsers%{$_|getType|equal'ParserKind.combinator}@Fsharp.lang"
    match res with
    | Parser.Success (l, _) ->
        printEmailTree l
    | Parser.Failure _ -> Parser.printResult res
    Console.ReadKey(true) |> ignore
    0
