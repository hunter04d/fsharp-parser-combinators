// Learn more about F# at http://fsharp.org
open System
open EmailParser
open Parser
open System.Text
open System.Text.RegularExpressions
let printEmailTree list: unit =
    let intent = "    "

    let emailToJsonLike(localPart: string,domainPart) =
        let localPartEscaped = localPart.Replace("\"", "\\\"")
        let escape s: string = sprintf "\"%s\"" s
        let res = StringBuilder()
        res.AppendLine("{")
           .AppendLine(sprintf "%s\"local part\": \"%s\"," intent localPartEscaped )
           .Append(sprintf "%s\"domain part\":\n%s[\n%s" intent intent intent)
           .AppendJoin(",\n    ",
                       (List.map (escape >> (fun s -> intent + s)) domainPart)
                       |> List.toSeq).AppendLine("\n    ]").Append("}")
           .ToString()
    printfn "["
    printfn "%s" (String.concat ",\n" (list
                                       |> List.map emailToJsonLike
                                       |> List.toSeq))
    printfn "]"

[<EntryPoint>]
let main argv =
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let res = emailList |> Parser.runOnFile @"emails.txt"
    timer.Stop()
    match res with
    | Parser.Success(l,_) -> printEmailTree l
    | Parser.Failure _ -> Parser.printResult res
    printfn "parse took %f ms" timer.Elapsed.TotalMilliseconds
    Console.ReadKey(true) |> ignore
    0
