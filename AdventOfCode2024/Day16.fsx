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
type Direction =
    | North = 0
    | East = 1
    | South = 2
    | West = 3
type Cost = int
type Position = Position of Point * Direction
type Neighbour = Neighbour of Position * Cost
type Graph = Map<Position, Neighbour list>

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
    
let parse input =
    let arr = parseArray2D input 0 ' '
    let startTile = AdventArray.findAndClear 'S' arr '.'
    let endTile = AdventArray.findAndClear 'E' arr '.'
    let maxx, maxy = AdventArray.maxes arr 0
    let getPositions point =
        let facings = [for di in 0..3 do let v = directions4[di] in (di, addvec point v)]
        let neighbours position =
            let (Position (point, d)) = position
            let turns = [for dd in 0..3 do let dd = enum<Direction> dd in if dd <> d then Neighbour (Position (point, dd), 1000)]
            let (Point np) = let v = directions4[int d] in addvec point v
            match arr[fst np, snd np] with
            | '.' -> turns @ [Neighbour (Position (Point np, d), 1)]
            | '#' -> turns
            | c -> failwith (string c)
        [for di in 0..3 do enum<Direction> di] |> List.map (fun d -> let pos = Position (point, d) in (pos, neighbours pos))
    let graph = [for x in 0..maxx do for y in 0..maxy do if arr[x, y] = '.' then yield Point(x, y)] |> List.collect getPositions |> Map.ofList
    graph, Position (Point startTile, Direction.East), Point endTile

let dumpArray enable arr p i m=
    if enable then
        let maxx, maxy = AdventArray.maxes arr 0
        for y in [0..maxy] do
            for x in [0..maxx] do
                let c = if (Point (x, y) = p) then m else arr[x, y]
                printf $"{c}"
            printf "\n"
        printfn $"{i}:{m}"
        
let calc part2 input=
    let (graph:Graph), (startPos:Position), (endTile:Point) = input
    
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
            elif alt = dist[v] && not (prev[v] |> List.contains u) then
                // for part 2
                prev <- prev |> Map.add v (u :: Map.find v prev)
                dist <- dist |> Map.add v alt
                queue.Enqueue(v, alt)

    let shortestPath = dist |> Map.filter (fun (Position (p, _)) _ -> p = endTile) |> Map.toSeq |> Seq.minBy snd
    let shortestLength = snd shortestPath
    let shortestEndPositions = dist |> Map.toSeq |> Seq.filter (fun (Position (p, _), l) -> p = endTile && l = shortestLength) |> Seq.map fst |> List.ofSeq
    let rec visitLoop visited toVisit =
        match toVisit with
        | v :: rest ->
            if v = startPos then
                visitLoop (Set.add v visited) rest
            elif not (Set.contains v visited) then
                // printfn $"Prev[{v}]={prev[v]}"
                visitLoop (Set.add v visited) (prev[v] @ rest)
            else
                visitLoop visited rest
        | [] -> visited
    let tilesOnShortestPaths = visitLoop Set.empty shortestEndPositions |> Set.map (fun (Position (p, _)) -> p)
    let goodTiles = tilesOnShortestPaths.Count
    if part2 then goodTiles else shortestLength

let calc1 = calc false
let calc2 = calc true
let test =
    let sw = Stopwatch.StartNew()
    let day = 16

    let test0 = """
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
"""  
    calc1 (parse test0) |> should equal 7036
    
    let test1 = """
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
"""  
    calc1 (parse test1) |> should equal 11048
    
    let input = readDailyInput day
    calc1 (parse input) |> should equal 133584

    calc2 (parse test0) |> should equal 45
    calc2 (parse test1) |> should equal 64
    calc2 (parse input) |> should equal 622

    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
