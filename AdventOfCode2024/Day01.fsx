#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"

open Utility
open System
open FsUnit.Xunit

type Input = int list * int list

let parse (inputText:string) =
    inputText |> parseLines |> List.map split2space |> List.map (fun (s1, s2) -> (int s1, int s2)) |> List.unzip

let calc1 (input:Input) =
    let l1, l2 = input
    List.zip (l1 |> List.sort) (l2 |> List.sort) |> List.map (fun (i1, i2) -> abs(i1-i2)) |> List.sum

let calc2 (input:Input) =
    let l1, l2 = input
    let countMap = List.countBy id l2 |> Map.ofList
    let similarityScore n =
        match Map.tryFind n countMap with
        | Some c -> n * c
        | None -> 0
    l1 |> List.sumBy similarityScore

let test =
    let day = 1
    let input1 = parse "3   4
4   3
2   5
1   3
3   9
3   3"
    calc1 input1 |> should equal 11

    let input = parse (readDailyInput day)
    calc1 input |> should equal 2196996

    calc2 input1 |> should equal 31
    calc2 input |> should equal 23655822
    
test    
