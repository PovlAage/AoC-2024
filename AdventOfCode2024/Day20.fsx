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
open FsUnit.CustomMatchers
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Quotations
open Utility
open FsUnit.Xunit

let parse input =
    let charArray = parseArray2D input 0 ' '
    let intArray = Array2D.create (Array2D.length1 charArray) (Array2D.length2 charArray) -1
    let startPos = AdventArray.findAndClear 'S' charArray '.'
    let endPos = AdventArray.findAndClear 'E' charArray '.'
    let rec loop p dist =
        let px, py = fst p, snd p
        intArray[px, py] <- dist
        match directions4 |> List.map (addvec p) |> List.tryFind (fun (xx, yy) -> charArray[xx, yy] = '.' && intArray[xx, yy] = -1) with
        | Some pNext -> assert (p <> startPos); loop pNext (dist + 1)
        | None -> assert (p = startPos); ()
    loop endPos 0
    intArray

let findCheats cheatlength input =
    let maxx, maxy = maxes input 0
    let cheatNeighbours =
        List.allPairs [-cheatlength..cheatlength] [-cheatlength..cheatlength] |> List.map (fun (dx, dy) -> (dx, dy), abs dx + abs dy) |> List.filter (fun (_, dist) -> 0 < dist && dist <= cheatlength)
    let cheats p =
        let startDist = input[fst p, snd p]
        if startDist = -1 then
            []
        else
            let isInbounds (x, y) = 0 < x && x < maxx && 0 < y && y < maxy && input[x, y] <> -1
            let saving ((x, y), dist) = startDist - (input[x, y] + dist)
            cheatNeighbours |> List.map (fun (v, dist) -> (addvec p v, dist)) |> List.filter (fun (p, _) -> isInbounds p) |> List.map (fun p2 -> (p, fst p2, saving p2)) |> List.filter (fun (_, _, saving) -> saving > 0)
        
    [for x in 0..maxx do for y in 0..maxy do (x, y)] |> List.collect cheats

let calc cheatLength input  min=
    let cheats = findCheats cheatLength input |> List.filter (fun (_, _, saving) -> saving >= min)
    cheats |> List.length

let calc1 = calc 2
    
let calc2 = calc 20
    
let test =
    let sw = Stopwatch.StartNew()
    let day = 20

    let test0 = """
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
"""
    calc1 (parse test0) 40 |> should equal 2
    calc1 (parse test0) 41 |> should equal 1
    calc1 (parse test0) 64 |> should equal 1
    calc1 (parse test0) 65 |> should equal 0

    let input = readDailyInput day
    calc1 (parse input) 100 |> should equal 1332

    calc2 (parse test0) 74 |> should equal 7
    calc2 (parse test0) 76 |> should equal 3    
    calc2 (parse test0) 77 |> should equal 0    

    calc2 (parse input) 100 |> should equal 987695

    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
