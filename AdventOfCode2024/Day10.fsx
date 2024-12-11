#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"
#load "AdventArray.fs"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open AdventArray
open Utility
open FsUnit.Xunit

type Tile = Tile of int * int list

let parse (input:string) =
    let parr = parseIntArray2D input 1
    let maxx, maxy = maxes parr 1
    let tileId (x, y) = (x - 1) + (y - 1) * (Array2D.length1 parr - 2)
    let isValid v (x, y) = parr[x, y] = v + 1
    let validNbors (x, y) =
        let v = parr[x,y]
        [for dx, dy in directions4 do let nbor = addvec (x, y) (dx, dy) in if isValid v nbor then tileId nbor]
    let tiles = [for x in 1..maxx do
                 for y in 1..maxy do
                 let v = parr[x, y]
                 (tileId (x, y), Tile (v, (validNbors (x, y))))]
    let topomap = tiles |> Map.ofList
    let trailheads = tiles |> List.filter (fun (id, Tile (v, _)) -> v = 0) |> List.map fst
    topomap, trailheads

let score (topomap:Map<int,Tile>) trailHead =
    let visitedTiles = HashSet<int>()
    let visitedPeaks = HashSet<int>()
    let queue = Queue<int>()
    queue.Enqueue(trailHead)
    while queue.Count > 0 do
        let t = queue.Dequeue()
        if not (visitedTiles.Contains(t)) then
            visitedTiles.Add(t) |> ignore
            let (Tile (v, nbors)) = topomap[t]
            for nbor in nbors do
                if not (visitedTiles.Contains(nbor)) then
                    queue.Enqueue(nbor)
            if v = 9 then
                visitedPeaks.Add(t) |> ignore
    visitedPeaks.Count

[<TailCall>]
let rec rating topomap trailHead =
    let (Tile (v, ns)) = topomap |> Map.find trailHead
    if v = 9 then
        1
    else
        ns |> List.sumBy (rating topomap)

let calc1 input =
    let topomap, trailheads = input |> parse
    trailheads |> List.sumBy (score topomap)

let calc2 input =
    let topomap, trailheads = input |> parse
    trailheads |> List.sumBy (rating topomap)

let test =
    let sw = Stopwatch.StartNew()
    let day = 10
    
    let test0 = """
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
"""
    (calc1 test0) |> should equal 36

    let input = readDailyInput day
    
    calc1 input |> should equal 737

    calc2 test0 |> should equal 81
    
    calc2 input |> should equal 1619
    
    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
