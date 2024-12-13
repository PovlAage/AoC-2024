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

type Button = Button of int64 * int64
type Prize = Prize of int64 * int64
type Machine = Machine of Button * Button * Prize

let parseButton line =
    let re = Regex(@"Button (A|B): X\+(?<x>\d+), Y\+(?<y>\d+)")
    let m = re.Match(line)
    Button ((int64 m.Groups["x"].Value), (int64 m.Groups["y"].Value))
let parsePrize line =
    let re = Regex(@"Prize: X=(?<x>\d+), Y=(?<y>\d+)")
    let m = re.Match(line)
    Prize ((int64 m.Groups["x"].Value), (int64 m.Groups["y"].Value))
let parseMachine lines =
    assert (List.length lines = 3)
    let a = parseButton (lines[0])
    let b = parseButton (lines[1])
    let p = parsePrize (lines[2])
    Machine (a, b, p)
let parse input = parseBlocks input |> List.map parseMachine

let calc1 machines =
    let cost (Machine (Button (ax, ay), Button (bx, by), Prize (px, py))) =
        match [for ta in [0L..100L] do for tb in [0L..100L] do if ta*ax + tb*bx = px && ta*ay + tb * by = py then yield 3L*ta + tb] with
        | [] -> 0L
        | l -> List.min l
    machines |> List.sumBy cost

let calc2 machines =
    let add10000000000000 (Machine (a, b, (Prize (px, py)))) = Machine (a, b, Prize (10000000000000L + px, 10000000000000L + py))
    let machines = List.map add10000000000000 machines
    let cost (Machine (Button (ax, ay), Button (bx, by), Prize (px, py))) =
        let det = ax * by - ay * bx
        if det = 0 then
            0L
        else
            let axi = by
            let byi = ax
            let ayi = -ay
            let bxi = -bx
            let tx = axi * px + bxi * py
            let ty = ayi * px + byi * py
            if tx % det <> 0 || ty % det <> 0 then
                0L
            else
                (3L * tx + ty) / det
            
    machines |> List.sumBy cost

let test =
    let sw = Stopwatch.StartNew()
    let day = 13
    
    parseButton "Button A: X+94, Y+34" |> should equal (Button (94, 34))
    parsePrize "Prize: X=8400, Y=5400" |> should equal (Prize (8400, 5400))
    
    let test0 = parse """
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"""
    
    calc1 test0 |> should equal 480L
    
    let input = parse (readDailyInput day)
    calc1 input |> should equal 25751L

    calc2 test0 |> should equal 875318608908L

    calc2 input |> should equal 108528956728655L
    
    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
