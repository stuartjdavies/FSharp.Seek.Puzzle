module FSharp.Seek.Puzzle.Tests.SolutionTests

open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote
open System.IO
open FSharp.Seek.Puzzle.Solution
open System
open Microsoft.FSharp.Reflection

let SPEC_DIMENSION = (5,5)

[<TestCase("..\..\TestCaseA.txt")>]
[<TestCase("..\..\TestCaseB.txt")>]
[<TestCase("..\..\TestCaseC.txt")>]
let ``Verify the test cases given in the specification``(fileName) =         
        let testData = File.ReadAllLines(fileName)
        let cmds = testData.[0 .. testData.Length - 2 ] 
        let output = testData.[testData.Length - 1]         
        cmds |> runCommands SPEC_DIMENSION |> (fun r -> r.Value) |> should equal output

let ``Verify that toy robot must not fall off the table during movement. This also includes the initial``() = ()

[<Test>]       
let ``When REPORT is called when no actions have been issued the result should be None``() =              
        [| "REPORT" |] |> runCommands SPEC_DIMENSION |> should equal None

[<Test>]
let ``When REPORT is called after two invalid actions the result should be None``() =              
        [| "sdfsdf"; "sdfsdf"; "REPORT" |] |> runCommands SPEC_DIMENSION |> should equal None

let ``After placing the robot in two valid and a invalid coord the result is the last valid coordinates ``() =
         [| "PLACE 1,2,NORTH"; "PLACE 5,4,EAST"; "PLACE 10,10,SOUTH"; "REPORT" |] 
         |> runCommands SPEC_DIMENSION |> (fun r -> r.Value) |> should equal { ToyRobot.Position= { X=5; Y=4 }; Facing=South }


let getUnionCaseName (x:'a) = 
            match FSharpValue.GetUnionFields(x, typeof<'a>) with
            | case, _ -> case.Name          

let getFaceFromRandomNumber n =        
        match ((if n < 0 then n * -1 else n) % 4)  with
        | 1 -> North
        | 2 -> East
        | 3 -> South
        | 4 -> West 
        | _ -> failwith(sprintf "Invalid Face number %d" n)       
       
[<Test>]
let ``Use QuickCheck to place robot in various spots and validate the result against another range check algorithm``() =                      
          let testPlace x y f =                      
                  let f = getFaceFromRandomNumber f
                  let action = sprintf "PLACE %d,%d,%s" x y (getUnionCaseName(f))
                  let result = if x >= 0 && x <= (fst SPEC_DIMENSION) && (y >= 0 && y <= snd SPEC_DIMENSION) then
                                    Some({ ToyRobot.Position= { X=5; Y=4 }; Facing=f })
                               else
                                    None                               
                  [|action|] |> runCommands SPEC_DIMENSION |> should equal result
                                          
          Check.Quick testPlace         