#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"

open System
open System.IO
open FsUnit.Xunit

type Input = int list * int list

let parse (inputText:string) =
    let lines = inputText.Split(Environment.NewLine) |> List.ofArray |> List.filter (fun s -> not (String.IsNullOrEmpty s))
    let splitLine2 (line:string) =
        match line.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) with
            | [| e1; e2 |] -> (e1, e2)
            | _ -> failwith $"fail: {line}"
    lines |> List.map splitLine2 |> List.map (fun (s1, s2) -> (Int32.Parse s1, Int32.Parse s2)) |> List.unzip

let calc1 (input:Input) =
    let l1, l2 = input
    List.zip (l1 |> List.sort) (l2 |> List.sort) |> List.map (fun (i1, i2) -> abs(i1-i2)) |> List.sum

let calc2 (input:Input) =
    let l1, l2 = input
    let countMap = List.countBy id l2 |> Map.ofList
    let similarityScore n =
        let count =
            match countMap |> Map.tryFind n with
            | Some c -> c
            | None -> 0
        n * count
    l1 |> List.sumBy similarityScore

let test =
    let input1 = parse "3   4
4   3
2   5
1   3
3   9
3   3"
    calc1 input1 |> should equal 11

    let input = parse (File.ReadAllText(Path.Combine("AdventOfCode2024", "Input", "input01.txt")))
    calc1 input |> should equal 2196996

    calc2 input1 |> should equal 31
    calc2 input |> should equal 23655822
    
test    
