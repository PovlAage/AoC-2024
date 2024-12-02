module Utility

open System
open System.IO

let readDailyInput day =
    let path = Path.Combine("AdventOfCode2024", "Input", $"input{day:D2}")
    File.ReadAllText(path)

let getLines (inputText:string) =
    inputText.Split(Environment.NewLine) |> List.ofArray |> List.filter (fun s -> not (String.IsNullOrEmpty s))

let split2 (c:char) (s:string) =
    match s.Split(c, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) with
    | [| x ; y |] -> x, y
    | _ -> failwithf $"Could not split {s} into two"

let split2space = split2 ' '

let splitSpace (s:string) =
    s.Split (' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
