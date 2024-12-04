module Utility

open System
open System.IO

let readDailyInput day =
    let path = Path.Combine("AdventOfCode2024", "Input", $"input{day:D2}")
    File.ReadAllText(path)

let parseLines (inputText:string) =
    inputText.Split(Environment.NewLine) |> List.ofArray |> List.filter (fun s -> not (String.IsNullOrEmpty s))

let parseArray2D inputText padding =
    let lines = parseLines inputText
    let ydim = lines.Length
    let xdim = lines |> List.map String.length |> List.distinct |> List.exactlyOne
    let arr = Array2D.init xdim ydim (fun x y -> lines[y][x])
    if padding = 0 then
        arr
    else
        let arr2 = Array2D.create (xdim + 2 * padding) (ydim + 2 * padding) '.'
        Array2D.blit arr 0 0 arr2 padding padding xdim ydim
        arr2

let split2 (c:char) (s:string) =
    match s.Split(c, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) with
    | [| x ; y |] -> x, y
    | _ -> failwithf $"Could not split {s} into two"

let split2space = split2 ' '

let splitSpace (s:string) =
    s.Split (' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
