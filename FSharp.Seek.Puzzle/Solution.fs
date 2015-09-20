module FSharp.Seek.Puzzle.Solution

open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

type Unit = int
type SquareTableDimensions = Unit * Unit 
type Coordinate = { X : Unit; Y : Unit } 
type DirectionFaced = North | East | South | West 
type ToyRobot = { Position : Coordinate; Facing : DirectionFaced; }
type Command = | Place of Coordinate * DirectionFaced | Move | Left | Right | Report | InvalidCommand

let getCommandFromString (s : string) =
        let parseDirection s = match s with 
                               | "NORTH" -> North 
                               | "EAST" -> East 
                               | "SOUTH" -> South 
                               | "WEST" -> West 
                               | _ -> failwith(sprintf "Invalid direction %s" s)
        let parsePlaceCommand (s : string) =                    
                if s.StartsWith("PLACE") then
                        try
                            let items = s.Replace("PLACE", String.Empty).Trim().Split(',')             
                            Place({ Coordinate.X=Int32.Parse(items.[0]); Y=Int32.Parse(items.[1]); }, parseDirection items.[2])
                        with
                        | _ -> InvalidCommand
                else
                   InvalidCommand
        match s with
        | _ when s.StartsWith("PLACE") -> parsePlaceCommand s
        | "MOVE" -> Move
        | "RIGHT" -> Right
        | "LEFT" -> Left
        | "REPORT" -> Report        
        | _ -> InvalidCommand

let makeMove (r : ToyRobot option) (d : SquareTableDimensions) (line : string) =              
        let isValidCoordinate coord dim = (coord.X >= 0) && (coord.X <= fst dim) && (coord.Y >= 0) && (coord.Y <= snd dim)
        let placeRobot coord face dim = if isValidCoordinate coord dim = true then Some( { ToyRobot.Facing = face; Position = coord; }) else None 
        let makeFirstMove dim cmd =        
                match cmd with
                | Place(coord, face) -> placeRobot coord face dim
                | _ -> None
        let makeNextMove robot dim cmd = 
                let rotateRobotLeft robot = { robot with Facing = match robot.Facing with | North -> West | East -> North | South -> East | West -> South }
                let rotateRobotRight robot = { robot with Facing = match robot.Facing with | North -> East | East -> South | South -> West | West -> North }
                let moveRobot robot dim =
                          let nextPosition = match robot.Facing with 
                                             | North -> { robot.Position with Y=robot.Position.Y + 1 } 
                                             | East -> { robot.Position with X=robot.Position.X + 1 } 
                                             | South -> { robot.Position with Y=robot.Position.Y - 1 } 
                                             | West -> { robot.Position with X=robot.Position.X - 1 }                                                         
                          if (isValidCoordinate nextPosition dim) then Some({ robot with Position =nextPosition; }) else None                                                           
                match cmd with
                | Place(coord, face) -> placeRobot coord face dim
                | Move -> moveRobot robot dim
                | Right -> Some(rotateRobotRight robot)
                | Left -> Some(rotateRobotLeft robot)                      
                | _ -> Some(robot)
        let cmd = getCommandFromString line
        match r with
        | Some r -> makeNextMove r d cmd
        | None -> makeFirstMove d cmd

let runCommands (d : SquareTableDimensions) (lines : string seq)  =
        let getUnionCaseName (x:'a) = 
            match FSharpValue.GetUnionFields(x, typeof<'a>) with
            | case, _ -> case.Name                        
        let result = lines |> Seq.map(fun line -> line.Trim())
                           |> Seq.takeWhile(fun line -> line <> "REPORT")
                           |> Seq.fold(fun acc line -> makeMove acc d line) None 
        match result with
        | Some(r) -> Some(sprintf "Output: %d,%d,%s" r.Position.X r.Position.Y (getUnionCaseName(r.Facing).ToUpper()))
        | None -> None 

let readCommandFromConsole() = seq { while(true) do yield Console.ReadLine() }

