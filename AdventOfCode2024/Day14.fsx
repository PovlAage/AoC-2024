#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"
#load "AdventArray.fs"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Numerics
open System.Text.RegularExpressions
open AdventArray
open Utility
open FsUnit.Xunit

type Point = Point of (int * int)
type Vec = Vec of (int * int)
type Robot = Robot of Point * Vec
type Space = Space of (int * int)
let addvec (Point p) (Vec v) = Point (addvec p v)

let parseRobot (line:string) =
    let re = Regex(@"p=(?<px>[\-0-9]+),(?<py>[\-0-9]+)\sv=(?<vx>[\-0-9]+),(?<vy>[\-0-9]+)")
    let m = re.Match(line)
    let getInt (name:string) = int m.Groups[name].Value
    let p = Point (getInt "px", getInt "py")
    let v = Vec (getInt "vx", getInt "vy")
    Robot (p, v)

let parseRobots = parseLines >> List.map parseRobot

let quad (Space (sx, sy)) (Robot (Point (px, py), _)) =
    let isMid p s = 2 * p = s - 1
    let half p s = if p < s / 2 then 0 else 1

    if (isMid px sx || isMid py sy) then None else Some ((half px sx) + 2 * half py sy)

let safetyFactor robots space =
    robots |> List.countBy (quad space) |> List.filter (fun x -> (fst x).IsSome) |> List.map snd |> List.fold (*) 1

let stepRobot (Space (sx, sy)) (Robot (p, v)) =
    let (Point (px, py)) = addvec p v
    Robot (Point ((px + sx) % sx, (py + sy) % sy), v)

let calc n space robots print =
    let (Space (sx, sy)) = space
    let stepRobots = List.map (stepRobot space)
    let printAndStepRobots robots i =
        let robots = stepRobots robots
        let positions = robots |> Seq.map (fun (Robot (Point (px, py), _)) -> (px, py)) |> Set.ofSeq
        let isInlineWith n (x, y) =
            [1..n] |> List.forall (fun i -> positions.Contains((x, y + i)))
        
        if positions |> Set.exists (isInlineWith 10) then
            let arr = Array2D.create sx sy ' '
            for Robot (Point (px, py), _) in robots do
                arr[px, py] <- 'X'
            printfn $"Step {i}:"
            for y in 0..(sy - 1) do
                for x in 0..(sx - 1) do
                    let c = if positions.Contains((x, y)) then 'X' else ' '
                    printf $"{c}"
                printfn ""
        robots
    
    let folder = if print then printAndStepRobots else (fun robots i -> stepRobots robots)
    let endState = [1..n] |> List.fold folder robots
    safetyFactor endState space

let calc1 space robots =
    calc 100 space robots false

let calc2 n space robots =
    calc n space robots true |> ignore

let test =
    let sw = Stopwatch.StartNew()
    let day = 14

    let v0 = Vec(2,-3)
    let testRobot0 = Robot (Point(2,4), v0)
    let testSpace0 = (Space (11, 7))
    stepRobot testSpace0 testRobot0 |> should equal (Robot (Point(4,1), v0))
    stepRobot testSpace0 (stepRobot testSpace0 testRobot0) |> should equal (Robot (Point(6,5), v0))

    quad testSpace0 (Robot (Point (0, 0), v0)) |> should equal (Some 0)
    quad testSpace0 (Robot (Point (4, 2), v0)) |> should equal (Some 0)
    quad testSpace0 (Robot (Point (5, 2), v0)) |> should equal None
    quad testSpace0 (Robot (Point (4, 3), v0)) |> should equal None
    quad testSpace0 (Robot (Point (5, 3), v0)) |> should equal None
    quad testSpace0 (Robot (Point (6, 2), v0)) |> should equal (Some 1)
    quad testSpace0 (Robot (Point (4, 4), v0)) |> should equal (Some 2)
    quad testSpace0 (Robot (Point (6, 4), v0)) |> should equal (Some 3)

    let test0 = parseRobots """
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
"""  
    calc1 testSpace0 test0 |> should equal 12
    
    let space = Space (101, 103)
    let input = parseRobots (readDailyInput day)
    calc1 space input |> should equal 229868730

    calc2 10000 space input

    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
