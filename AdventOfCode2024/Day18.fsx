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
open Utility
open FsUnit.Xunit

type Point = Point of (int * int)
type Vec = Vec of (int * int)
type Direction =
    | North = 0
    | East = 1
    | South = 2
    | West = 3
type Cost = int
type Position = Position of Point
type Neighbour = Neighbour of Position * Cost
type Graph = Map<Position, Neighbour list>

let parse input =
    let lines = parseLines input
    lines |> List.map (split2 ',') |> List.map (fun (a, b) -> int a, int b)

let addvec (Point p) (Vec v) = Point (addvec p v)
let scalevec (Vec v) c = Vec (c * fst v, c * snd v)
let directions4 = directions4 |> List.map Vec
let m2v c =
    match c with
    | '^' -> directions4[0]
    | '>' -> directions4[1]
    | 'v' -> directions4[2]
    | '<' -> directions4[3]
    | _ -> failwith $"Invalid {c}"
    
let parseGraph startPoint endPoint arr =
    let maxx, maxy = AdventArray.maxes arr 0
    let isFree (Point p) = arr[fst p, snd p] = '.'
    let getPositions point =
        let neighbours = directions4 |> List.map (fun v -> addvec point v) |> List.filter isFree |> List.map (fun np -> Neighbour (Position np, 1))
        (Position point), neighbours
    let graph = [for x in 0..maxx do for y in 0..maxy do if arr[x, y] = '.' then yield Point(x, y)] |> List.map getPositions |> Map.ofList
    graph, Position startPoint, Position endPoint
    
let dijkstra input=
    let (graph:Graph), (startPos:Position), (endPos:Position) = input

    // init
    let vertices = graph |> Map.toList |> List.map fst
    let mutable dist = vertices |> Seq.map (fun v -> v, Int32.MaxValue) |> Map.ofSeq
    dist <- dist.Add(startPos, 0)
    let mutable prev = Map.empty            
    let queue = PriorityQueue<Position, int>()
    queue.Enqueue(startPos, 0)
    
    while queue.Count > 0 do
        let u = queue.Dequeue()
        let neighbours = graph |> Map.find u
        for Neighbour (v, d) in neighbours do
            let alt = dist[u] + d
            if alt < dist[v] then
                prev <- prev |> Map.add v [u]
                dist <- dist |> Map.add v alt
                queue.Enqueue(v, alt)

    let shortestPath = snd (dist |> Map.toSeq |> Seq.filter (fun (pos, _) -> pos = endPos) |> Seq.minBy snd)
    if shortestPath = Int32.MaxValue then
        None
    else
        Some shortestPath

let calc1 dims bytes=
    let xdim, ydim = fst dims, snd dims
    let arr = Array2D.create (xdim + 2) (ydim + 2) '.'
    for xborder in [0;xdim + 1] do
        for y in [0..ydim + 1] do
            arr[xborder, y] <- '#'
    for yborder in [0;ydim + 1] do
        for x in [0..xdim + 1] do
            arr[x, yborder] <- '#'
    for x, y in bytes do
        arr[x + 1, y + 1] <- '#'
    let graph = parseGraph (Point (1, 1)) (Point (xdim, ydim)) arr
    dijkstra graph

let calc2 dims input =
    let rec binary_search cache pmin pmax =
        let hasExit cache p =
            match cache |> Map.tryFind p with
            | Some v -> v, cache
            | None ->
                let v = (calc1 dims (input |> List.take p)).IsSome
                v, cache.Add(p, v)
        let guess = (pmin + pmax) / 2
        let vminus, cache = hasExit cache (guess - 1)
        let v, cache = hasExit cache guess
        if vminus && not v then
            guess
        else
            if not v then
                binary_search cache pmin (guess - 1)
            else
                binary_search cache (guess + 1) pmax
    input[binary_search Map.empty 1 (List.length input) - 1]

let test =
    let sw = Stopwatch.StartNew()
    let day = 18

    let test0 = """
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
"""
    calc1 (7,7) ((parse test0) |> List.take 12) |> should equal (Some 22)
    
    let input = readDailyInput day
    calc1 (71,71) ((parse input) |> List.take 1024) |> should equal (Some 416)

    calc2 (7,7) (parse test0) |> should equal (6, 1)
    calc2 (71,71) (parse input) |> should equal (50, 23)

    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
