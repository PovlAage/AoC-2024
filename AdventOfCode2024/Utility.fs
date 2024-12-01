module Utility

open System
open System.IO

let readDailyInput day =
    let path = Path.Combine("AdventOfCode2024", "Input", $"input{day:D2}.txt")
    File.ReadAllText(path)

let getLines (inputText:string) =
    inputText.Split(Environment.NewLine) |> List.ofArray |> List.filter (fun s -> not (String.IsNullOrEmpty s))
