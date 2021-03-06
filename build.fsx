#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.create "Clean" (fun _ ->
    !! "*/bin"
    ++ "*/obj"
    |> Shell.cleanDirs
)

Target.create "Build" (fun _ ->
    !! "./**/*.*proj"
    |> Seq.iter (DotNet.build id)
)
Target.create "Run" (fun _ ->
    DotNet.exec id "run" "-p ./src" |> ignore
)


Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "All"

"Build" ==> "Run"
Target.runOrDefault "All"
