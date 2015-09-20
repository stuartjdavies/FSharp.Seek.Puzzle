open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection
open FSharp.Seek.Puzzle.Solution

[<EntryPoint>]
let main argv = 
    let SPEC_DIMENSION = (5,5)
    readCommandFromConsole() |> runCommands SPEC_DIMENSION |> (fun output -> match output with 
                                                                             | Some(x) -> printfn "%s" x
                                                                             | None -> printfn "Output: None")
    Console.ReadLine() |> ignore
    0 // return an integer exit code
