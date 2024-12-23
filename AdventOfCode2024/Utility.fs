module Utility

open System
open System.IO

let readDailyInput day =
    let path = Path.Combine("AdventOfCode2024", "Input", $"input{day:D2}")
    File.ReadAllText(path)

let parseLines (inputText:string) =
    inputText.Split(Environment.NewLine) |> List.ofArray |> List.map (fun s -> s.Trim()) |> List.filter (fun s -> not (String.IsNullOrEmpty s))

let parseBlocks (input:string) =
    let blocks = input.Split("\n\n") |> List.ofArray
    blocks |> List.map parseLines

let parseBlocks2 input =
    let blocks = parseBlocks input
    match blocks with
    | [b1;b2] -> b1, b2
    | _ -> printfn $"%A{blocks}" ; failwith $"Bad number of blocks: {blocks.Length}"

let split2 (c:char) (s:string) =
    match s.Split(c, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) with
    | [| x ; y |] -> x, y
    | _ -> failwithf $"Could not split {s} into two"

let split2space = split2 ' '

let splitSpace (s:string) =
    s.Split (' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

let removeMany keys map =
    keys |> Seq.fold (fun m c -> m |> Map.remove c) map
