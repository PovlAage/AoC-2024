#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"
#load "AdventArray.fs"

open System.Diagnostics
open AdventArray
open Utility
open FsUnit.Xunit

type Frequency = char
type Point = int * int
type Antenna = Antenna of Frequency*Point
type Node = Node of Point

let isInBounds maxx maxy p =
    let x, y = fst p, snd p
    0 <= x && x <= maxx && 0 <= y && y <= maxy

let getAntiNodes isPart2 maxx maxy positions=
    let getAntiNodesForPair (p1, p2) =
        let dx, dy = vec p1 p2
        let s = if isPart2 then Seq.initInfinite (fun i -> addvec p2 (i * dx, i * dy)) else Seq.singleton (addvec p2 (dx, dy))
        s |> Seq.takeWhile (isInBounds maxx maxy) |> List.ofSeq
    List.allPairs positions positions |> List.filter (fun (p1, p2) -> p1 <> p2) |> List.collect getAntiNodesForPair |> Set.ofList

let calc1 input  isPart2=
    let arr = parseArray2D input 0 ' '
    let maxx, maxy = (Array2D.length1 arr - 1), (Array2D.length2 arr - 1)
    let antennae = [for x in 0..Array2D.length1 arr - 1 do for y in 0..Array2D.length2 arr - 1 do if arr[x, y] <> '.' then Antenna (arr[x,y], (x, y))]
    let antennaeGrouped = antennae |> List.groupBy (fun (Antenna (f, p)) -> f)
    let getAntiNodes = List.map (fun (Antenna (f, p)) -> p) >> getAntiNodes isPart2 maxx maxy
    antennaeGrouped |> List.map (snd >> getAntiNodes) |> Set.unionMany |> Set.count

let test =
    let sw = Stopwatch.StartNew()
    let day = 8
    
    getAntiNodes false 9 9 [(4, 3); (5, 5)]|> should equal (Set.ofList [(6, 7); (3, 1)])
    getAntiNodes false 9 9 [(4, 3); (5, 5); (8, 4)] |> should equal (Set.ofList [(6, 7); (3, 1); (0, 2); (2, 6)])
    getAntiNodes false 11 11 [(6, 5); (8, 8); (9, 9)] |> should equal (Set.ofList [(3, 1); (4, 2); (7, 7); (10, 10); (10, 11)])
    let test1 = """
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"""
    calc1 test1 false |> should equal 14
    
    let input = readDailyInput day
    calc1 input false |> should equal 276

    calc1 test1 true |> should equal 34
    calc1 input true |> should equal 991
    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
