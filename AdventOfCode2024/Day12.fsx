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

type Point = Point of (int * int)
type Vec = Vec of (int * int)
let addvec (Point p) (Vec v) = Point (addvec p v)
type Region = char * Set<Point> 

let parse input = parseArray2D input 1 ' '
let getRegion (arr:char array2d) (Point (x, y)) =
    let c = arr[x, y]
    let mutable elements = Set.empty
    let mutable toVisit = Queue<int*int>()
    toVisit.Enqueue(x, y)
    while toVisit.Count > 0 do
        let v = Point (toVisit.Dequeue())
        if not (Set.contains v elements) then
            elements <- Set.add v elements
            for d in directions4 do
                let (Point nbor) = addvec v (Vec d)
                if arr[fst nbor, snd nbor] = c && not (Set.contains (Point nbor) elements) then
                    toVisit.Enqueue(nbor)
    c, Set.ofSeq(elements)

let getRegions arr =
    let maxx, maxy = maxes arr 1
    let mutable regions = []
    let isAlreadyInRegion p (r:Region) =
        let points = snd r
        points |> Set.contains p
    let isAlreadyInAnyRegion (rs:Region list) p =
        rs |> List.exists (isAlreadyInRegion p)
        
    for x in 1..maxx do
        for y in 1..maxy do
            let p = Point (x, y)
            if not (isAlreadyInAnyRegion regions p) then regions <- (getRegion arr p) :: regions

    List.ofSeq regions

let area (region:Region) =
    (snd region).Count

let isOutside (region:Region) p =
    not ((snd region).Contains(p))

let perimeter (region:Region) =
    let reg = snd region
    let countPerimeter t =
        directions4 |> List.map (Vec >> addvec t) |> List.filter (isOutside region) |> List.length
    reg |> Seq.sumBy countPerimeter

let perimeter2 (region:Region) =
    let normals = (snd region) |> Seq.collect (
        fun t -> directions4 |> List.indexed |> List.map (fun (i, v) -> (i, Vec v)) |> List.filter (snd >> (addvec t) >> (isOutside region)) |> List.map (fun (i, d) -> (i, t))) |> Seq.groupBy fst |> Seq.map (fun (di, dt) -> di, (dt |> Seq.map snd |> Seq.toList)) |> Map.ofSeq
    let p2 nv tiles =
        let pv = Vec directions4[(nv + 1) % 4]
        let compare (Point p1) (Point p2) =
            let swap (x, y) = (y, x)
            match pv with
            | Vec (0, -1) -> compare p2 p1
            | Vec (1, 0) -> compare (swap p1) (swap p2)
            | Vec (0, 1) -> compare p1 p2
            | Vec (-1, 0) -> compare (swap p2) (swap p1)
            | _ -> failwithf $"Unexpected pv={pv}"
        1 + (tiles |> List.sortWith compare |> List.pairwise |> List.filter (fun (t1, t2) -> addvec t1 pv <> t2) |> List.length)

    normals |> Seq.sumBy (fun kv -> p2 kv.Key kv.Value)

let price pfun region  = (area region) * (pfun region)

let calc1 input =
    input |> List.sumBy (price perimeter)

let calc2 input =
    input |> List.sumBy (price perimeter2)

let test =
    let sw = Stopwatch.StartNew()
    let day = 12
    
    let test0 = parse """
AAAA
BBCD
BBCC
EEEC
"""
    
    let regions0 = getRegions test0
    [for r in regions0 do fst r] |> should equal ['D'; 'C'; 'E'; 'B'; 'A']
    [for r in regions0 do area r] |> should equal [1; 4; 3; 4; 4]
    [for r in regions0 do perimeter r] |> should equal [4; 10; 8; 8; 10]

    perimeter2 regions0[0] |> should equal 4
    perimeter2 regions0[1] |> should equal 8
    perimeter2 regions0[2] |> should equal 4
    perimeter2 regions0[3] |> should equal 4
    perimeter2 regions0[4] |> should equal 4

    let test2 = parse """
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
"""
    let regions2 = getRegions test2
    calc1 regions2 |> should equal 1930
    
    let input = parse (readDailyInput day)
    calc1 (getRegions input) |> should equal 1456082

    calc2 regions2 |> should equal 1206

    calc2 (getRegions input) |> should equal 872382
    
    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
