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

type NumKeyPad = char
type DirKeyPad = char
type StateKey = string
type ListState = DirKeyPad list * NumKeyPad
type Neighbour<'T> = Neighbour of 'T * int
type Node<'T> = { State: 'T; Neighbours: Neighbour<'T> list}
type Graph<'T when 'T : comparison> = Map<'T, Neighbour<'T> list> 
let listStateToArrayState (s:ListState) =
    let ds, n = s
    String.Join("", ds) + string n

let parse= parseLines
let act = 'A'
let numKeys = ([0..9] |> List.map string |> List.map (fun s -> s[0])) @ [act]
let dirKeysAndVecs = ['^', (0, -1); 'v', (0, 1); '<', (-1, 0); '>', (1, 0); act, (0, 0)]
let dirKeys = dirKeysAndVecs |> List.map fst
let keyToVec = dirKeysAndVecs |> Map.ofSeq
let numArrayStr = """
789
456
123
#0A
"""
let numArray = AdventArray.parseArray2D numArrayStr 1 '#'
let mutable numPos = Map.empty
numArray |> Array2D.iteri (fun x y c -> if c <> '#' then numPos <- numPos.Add((x, y), c))
let numPosRev = numPos |> Map.toSeq |> Seq.map (fun x -> snd x, fst x) |> Map.ofSeq
for x in numPos do printfn $"{x}"

let dirArrayStr = """
#^A
<v>
"""
let dirArray = AdventArray.parseArray2D dirArrayStr 1 '#'
let mutable dirPos = Map.empty
dirArray |> Array2D.iteri (fun x y c -> if c <> '#' then dirPos <- dirPos.Add((x, y), c))
let dirPosRev = dirPos |> Map.toSeq |> Seq.map (fun x -> snd x, fst x) |> Map.ofSeq
for x in dirPos do printfn $"{x}"

let tryMoveDirKey key dirKey =
    let pos = dirPosRev[key]
    let newPos = addvec pos keyToVec[dirKey]
    if dirPos.ContainsKey(newPos) then Some dirPos[newPos] else None

let tryMoveNumKey key dirKey =
    let pos = numPosRev[key]
    let newPos = addvec pos keyToVec[dirKey]
    if numPos.ContainsKey(newPos) then Some numPos[newPos] else None

let buildGraph_2 (upperDists:Map<char*char, int>) (lowerStates:char list) (tryMove:char -> char -> char option)=
    let upperStates = upperDists.Keys |> Seq.map fst |> List.ofSeq
    let states = List.allPairs upperStates lowerStates
    let getNeighbours (upper, lower) =
        let upperNeighbours = upperDists |> Seq.filter (fun kv -> fst kv.Key = upper) |> Seq.map (fun kv -> Neighbour ((snd kv.Key, lower), kv.Value))
        let lowerNeighbours = [tryMove lower upper] |> List.choose id |> List.map (fun l -> Neighbour ((upper, l), 1))
        lowerNeighbours @ List.ofSeq upperNeighbours
    let graph = states |> Seq.map (fun s -> s, getNeighbours s) |> Map.ofSeq
    let pureStates = lowerStates |> List.map (fun l -> (act, l))
    graph, pureStates
    
let buildGraph k =
    let pureStates = [for n in numKeys do (String.replicate k (string act) + string n)]

    let rec tryMoveStateList state dirKey =
        match state with
        | [], n ->
            match tryMoveNumKey n dirKey with
            | Some n2 when n2 <> n -> Some ([], n2)
            | _ -> None
        | d :: rest, n ->
            match tryMoveDirKey d dirKey with
            | Some d2 when d2 <> d -> Some (d2 :: rest, n)
            | Some d2 -> Option.map (fun (rest2, n2) -> (d2 :: rest2, n2)) (tryMoveStateList (rest, n) d2)
            | None -> None

    let rec loopStates acc kk =
        if kk = 0 then
            acc
        else
            loopStates (acc |> List.collect (fun x -> [for d in dirKeys do d :: x])) (kk - 1)

    let dirStates = loopStates [[]] k
    let states = [for n in numKeys do for ds in dirStates do (ds, n)]                                    
    let getNeighbours state =
        dirKeys |> List.choose (tryMoveStateList state)
    let graph = states |> Seq.map (fun s -> (listStateToArrayState s, [for n in getNeighbours s do Neighbour (listStateToArrayState n, 1)])) |> Map.ofSeq
    graph, pureStates
    
let dijkstra<'T when 'T : comparison> graph startPos=
    // init
    let vertices = graph |> Map.toList |> List.map fst
    let mutable dist = vertices |> Seq.map (fun v -> v, Int32.MaxValue) |> Map.ofSeq
    dist <- dist.Add(startPos, 0)
    let mutable prev = Map.empty            
    let queue = PriorityQueue<'T, int>()
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

    dist

let distMap k =
    let g, pureStates = buildGraph k
    // for gg in g do printfn $"{gg}"
    let pureState nk =
        if nk = act then pureStates[10] else pureStates[int(string nk)]
    // for kv in dijkstra (g, pureState '0') do printfn $"dijkstra {kv.Key}: {kv.Value}"
    Map.ofList [for n in numKeys do let ndist = dijkstra g (pureState n) in for m in numKeys do ((n, m), ndist[pureState m])]

let distMap_2 k =
    let distinctPairs l = List.allPairs l l |> List.filter (fun (x1, x2) -> x1 <> x2)
    let rec loop k dists =
        let lowerStates, tryMove = if k = 0 then numKeys, tryMoveNumKey else dirKeys, tryMoveDirKey
        let graph, pureStates = buildGraph_2 dists lowerStates tryMove
        let nextDists source =
            let dists = dijkstra graph source
            pureStates |> List.collect (fun s -> dists[source, s])
        loop (k - 1) (pureStates |> Seq.collect nextDists |> Map.ofSeq)
    let initialDists = distinctPairs dirKeys |> List.map (fun x -> (x, 1)) |> Map.ofList
    loop k initialDists
  
let shortest (dm:Map<char*char, int>) s =
    ("A" + s).ToCharArray() |> List.ofArray |> List.pairwise |> List.sumBy (fun (n, m) -> dm[(n, m)] + 1)

let calc k input =
    let dm = distMap k
    let numeric (s:string) =
        int64 (s.Substring(0, s.Length - 1))
        
    let complexity s =
        int64 (shortest dm s) * (numeric s)
    
    input |> List.sumBy complexity
    // let g = buildGraph n
    // let pureStates = (List.replicate )

let calc1 = calc 2
let calc2 = calc 25

let test =
    let sw = Stopwatch.StartNew()
    let day = 21

    // let d2 = distMap 2
    // for kv in d2 do printfn $"d2 {kv.Key}: {kv.Value}"
    // let d1 = distMap 1
    // for kv in d1 do printfn $"d1 {kv.Key}: {kv.Value}"
    // let d0 = distMap 0
    // for kv in d0 do printfn $"d0 {kv.Key}: {kv.Value}"

    // d0[('9', '8')] |> should equal 1
    
    let test0 = parse """
029A
980A
179A
456A
379A
"""
    let dm2 = distMap 2
    shortest dm2 test0[0] |> should equal 68
    shortest dm2 test0[1] |> should equal 60
    shortest dm2 test0[2] |> should equal 68
    shortest dm2 test0[3] |> should equal 64
    shortest dm2 test0[4] |> should equal 64

    calc1 test0 |> should equal 126384L

    let input = parse (readDailyInput day)
    
    calc1 input |> should equal 123096L

    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
