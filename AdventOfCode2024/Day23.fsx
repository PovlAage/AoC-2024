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
type Computer = string
type Connection = Computer * Computer
type Graph = Map<Computer, Set<Computer>>
let parseConnection (line:string) =
    (line |> split2 '-')
let parse input =
    let connections = input |> parseLines |> List.map parseConnection
    let connectionsSym = List.append connections (connections |> List.map (fun x -> snd x, fst x))
    connectionsSym |> List.groupBy fst |> List.map (fun (k, vs) -> (k, vs |> List.map snd |> Set.ofList)) |> Map.ofList

let calc1 (graph:Graph) =
    let hasT (x:Computer) = x[0] = 't'
    let get3clusters x =
        let ns = graph[x]
        let clusters = Seq.allPairs ns ns |> Seq.filter (fun (y1, y2) -> y1 < y2 && graph[y1] |> Set.contains y2)
        clusters |> Seq.map (fun (y1, y2) -> String.Join(",", List.sort [x; y1; y2]))
    graph.Keys |> Seq.filter hasT |> Seq.collect get3clusters |> Set.ofSeq |> Seq.length

let calc2 (graph:Graph) =
    let isInClique clique c = Set.isSubset clique graph[c]
    let rec loop visit (visited:Set<Computer>) acc =
        let rec expandClique candidates acc =
            match candidates with
            | [] -> acc
            | c :: rest when isInClique acc c -> expandClique rest (acc |> Set.add c)
            | _ :: rest -> expandClique rest acc
        match visit with
        | [] -> acc
        | v :: rest when visited.Contains(v) -> loop rest visited acc
        | v :: rest ->
            let maxClique = expandClique (graph[v] |> Set.toList) (Set.singleton v)
            loop rest (Set.union visited maxClique) (maxClique :: acc)
        
    let maxCliques = loop (graph.Keys |> List.ofSeq) Set.empty []
    let largestClique = maxCliques |> List.maxBy Set.count
    let password = String.Join(",", largestClique |> Set.toList |> List.sort)
    password

let test =
    let sw = Stopwatch.StartNew()
    let day = 23

    let test1 = parse """
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
"""
    calc1 test1 |> should equal 7
    let input = parse (readDailyInput day)
    calc1 input |> should equal 1368

    calc2 test1 |> should equal "co,de,ka,ta"
    calc2 input |> should equal "dd,ig,il,im,kb,kr,pe,ti,tv,vr,we,xu,zi"

    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"

test
printfn $"Done"
