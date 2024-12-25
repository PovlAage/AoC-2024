#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"
#load "AdventArray.fs"
#load "AdventGraph.fs"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Numerics
open System.Text.RegularExpressions
open AdventArray
open AdventGraph
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Quotations
open Utility
open FsUnit.Xunit

type NumKeyPad = char
type DirKeyPad = char
type StateKey = string
type ListState = DirKeyPad list * NumKeyPad
let listStateToArrayState (s:ListState) =
    let ds, n = s
    String.Join("", ds) + string n

let parse= parseLines

let buildGraph (upperDists:Map<char*char, int64>) (lowerStates:char list) (tryMove:char -> char -> char option)=
    let upperStates = upperDists.Keys |> Seq.map fst |> List.ofSeq
    let states = List.allPairs upperStates lowerStates
    let getNeighbours (upper, lower) =
        let upperNeighbours = upperDists |> Seq.filter (fun kv -> fst kv.Key = upper) |> Seq.map (fun kv -> Neighbour ((snd kv.Key, lower), kv.Value))
        let lowerNeighbours = [tryMove lower upper] |> List.choose id |> List.map (fun l -> Neighbour ((upper, l), 1))
        lowerNeighbours @ List.ofSeq upperNeighbours
    states |> Seq.map (fun s -> s, getNeighbours s) |> Map.ofSeq

let distMap depth =
    let dirArrayStr = """
    #^A
    <v>
    """
    let dirArray = AdventArray.parseArray2D dirArrayStr 1 '#'
    let mutable dirPos = Map.empty
    dirArray |> Array2D.iteri (fun x y c -> if c <> '#' then dirPos <- dirPos.Add((x, y), c))
    let dirPosRev = dirPos |> Map.toSeq |> Seq.map (fun x -> snd x, fst x) |> Map.ofSeq
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
    let ACT = 'A'
    let numKeys = ([0..9] |> List.map string |> List.map (fun s -> s[0])) @ [ACT]
    let dirKeysAndVecs = ['^', (0, -1); 'v', (0, 1); '<', (-1, 0); '>', (1, 0); ACT, (0, 0)]
    let dirKeys = dirKeysAndVecs |> List.map fst
    let keyToVec = dirKeysAndVecs |> Map.ofSeq
    let tryMoveDirKey key dirKey =
        let pos = dirPosRev[key]
        let newPos = addvec pos keyToVec[dirKey]
        if dirPos.ContainsKey(newPos) then Some dirPos[newPos] else None

    let tryMoveNumKey key dirKey =
        let pos = numPosRev[key]
        let newPos = addvec pos keyToVec[dirKey]
        if numPos.ContainsKey(newPos) then Some numPos[newPos] else None

    let distinctPairs l = List.allPairs l l |> List.filter (fun (x1, x2) -> x1 <> x2)
    let rec loop k dists =
        let lowerStates, tryMove = if k = 0 then numKeys, tryMoveNumKey else dirKeys, tryMoveDirKey
        let graph = buildGraph dists lowerStates tryMove
        let nextDistsFrom source =
            let distsFromSource = dijkstra graph (ACT, source)
            lowerStates |> List.filter ((<>) source) |> List.map (fun dest -> (source, dest), distsFromSource[(ACT, dest)])
        let nextDists = lowerStates |> List.collect nextDistsFrom |> Map.ofList
        if k = 0 then nextDists else loop (k - 1) nextDists
    let initialDists = distinctPairs dirKeys |> List.map (fun x -> (x, 0L)) |> Map.ofList
    loop depth initialDists
  
let shortest (dm:Map<char*char, int64>) s =
    ("A" + s).ToCharArray() |> List.ofArray |> List.pairwise |> List.sumBy (fun (n, m) -> dm[(n, m)] + 1L)

let calc k input =
    let dm = distMap k
    let numeric (s:string) =
        int64 (s.Substring(0, s.Length - 1))
        
    let complexity s =
        (int64 (shortest dm s)) * (numeric s)
    
    input |> List.sumBy complexity

let calc1 = calc 2
let calc2 = calc 25

let test =
    let sw = Stopwatch.StartNew()
    let day = 21

    let test0 = parse """
029A
980A
179A
456A
379A
"""
    let dm2 = distMap 2
    shortest dm2 test0[0] |> should equal 68L
    shortest dm2 test0[1] |> should equal 60L
    shortest dm2 test0[2] |> should equal 68L
    shortest dm2 test0[3] |> should equal 64L
    shortest dm2 test0[4] |> should equal 64L

    calc1 test0 |> should equal 126384L

    let input = parse (readDailyInput day)
    
    calc1 input |> should equal 123096L

    calc2 input |> should equal 154517692795352L

    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
