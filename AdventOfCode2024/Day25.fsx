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

type Key = Key of int list
type Lock = Lock of int list
type Input = int * Lock list * Key list

let isLock (block:string list) = block[0] = String.replicate block[0].Length "#"
let parseBlock (block:string list) =
    assert (isLock block)
    let len = block[0].Length
    let height i = block |> List.findIndex (fun line -> line[i] = '.')
    [0..len-1] |> List.map height

let parse input =
    let locks, keys = parseBlocks input |> List.partition isLock
    let height = locks[0].Length
    let locks = locks |> List.map (parseBlock >> Lock)
    let keys = keys |> List.map (List.rev >> parseBlock >> Key)
    height, locks, keys

let fit height (Lock lock) (Key key)=
    assert (lock.Length = key.Length)
    let length = lock.Length 
    List.zip lock key |> List.filter (fun (l, k) -> l + k <= height) |> List.length = length
    
let calc1 (input:Input) =
    let height, locks, keys = input
    List.allPairs locks keys |> List.filter (fun (l, k) -> fit height l k) |> List.length
    
let test =
    let sw = Stopwatch.StartNew()
    let day = 25

    let fit5 = fit 5
    fit5 (Lock [5;0;2;1;3]) (Key [0;5;3;4;3]) |> should equal false
    fit5 (Lock [4;3;4;0;2]) (Key [0;5;3;4;3]) |> should equal false
    fit5 (Lock [3;0;2;0;1]) (Key [0;5;3;4;3]) |> should equal true
    fit5 (Lock [5;0;2;1;3]) (Key [1;2;0;5;3]) |> should equal false
    fit5 (Lock [4;3;4;0;2]) (Key [1;2;0;5;3]) |> should equal true
    fit5 (Lock [3;0;2;0;1]) (Key [1;2;0;5;3]) |> should equal true

    let input = parse """
#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
"""
    calc1 input |> should equal 3

    let input = parse (readDailyInput day)
    calc1 input |> should equal 0

    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"

test
printfn $"Done"
