// Learn more about F# at http://fsharp.org

open System
open Parser
open EmailParser




[<EntryPoint>]
let main argv =
    let res = Parser.run emailList "ruskyeblyat@mail.ru faw@f"
    Parser.printResult res
    Console.ReadKey(true) |> ignore
    0
