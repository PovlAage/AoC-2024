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

type Pattern = string
type Patterns = Pattern list
type Design = string
type Designs = Design list

type Tree =
    | Node of string option * (string * Tree) list

let parse input =
    let patterns, designs = parseBlocks2 input
    let patterns = patterns[0].Split(", ") |> List.ofArray |> List.sort
    patterns, designs

let isPrefixOf (design:Design) (pattern:Pattern) = design.StartsWith(pattern)

let countPossible (patterns:Patterns) (design:Design) =
    assert (patterns = List.sort patterns)
    let getCandidates (pre, count) =
        let remaining = design[String.length pre..]
        patterns |> Seq.filter (isPrefixOf remaining) |> Seq.map (fun p -> (pre + p, count))

    let rec loop (candidates:Map<Design, int64>) acc =
        if candidates |> Map.containsKey design then
            loop (candidates |> Map.remove design) (acc + candidates[design])
        else
            if candidates.IsEmpty then
                acc
            else
                let newCandidates = (candidates |> Map.toSeq |> Seq.collect getCandidates |> Seq.groupBy fst |> Seq.map (fun (pre, counts) -> (pre, counts |> Seq.sumBy snd)) |> Map.ofSeq)
                loop newCandidates acc

    loop (Map [("", 1)]) 0L

let calcBoth (patterns, designs) =
    let counts = designs |> List.map (countPossible patterns)
    (counts |> List.filter (fun c -> c > 0) |> List.length), (counts |> List.sum)
    
let test =
    let sw = Stopwatch.StartNew()
    let day = 19

    let test0 = """
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
"""
    calcBoth (parse test0) |> should equal (6, 16L)

    let input = readDailyInput day
    calcBoth (parse input) |> should equal (304, 705756472327497L)

    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
