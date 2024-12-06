#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"
#load "AdventArray.fs"

open System.Text.RegularExpressions
open Utility
open AdventArray
open FsUnit.Xunit

let calc1 input =
    let arr = parseArray2D input 1 '.'
    let word = "XMAS"
    [
        for (dx, dy) in directions8 do
        for x in 1..(arr.GetLength(0)-2) do
        for y in 1..(arr.GetLength(1)-2) do
        if word.Length = (word.ToCharArray() |> List.ofArray |> List.indexed |> List.takeWhile (fun (i, c) -> arr[x + i * dx, y + i * dy] = c) |> List.length) then 1 else 0
    ] |> List.sum

let calc2 input =
    let arr = parseArray2D input 1 '.'
    let directions = [
        for dx in [-1;1] do for dy in [-1;1] do (dx, dy)    
    ]
    let word = "MAS"
    [
        for x in 1..(arr.GetLength(0)-2) do
        for y in 1..(arr.GetLength(1)-2) do
        if arr[x, y] = 'A' then
            let cornerLetters = [
                for (dx, dy) in directions do arr[x + dx, y + dy]
            ]
            if cornerLetters |> List.distinct |> List.sort = ['M'; 'S'] then
                if cornerLetters[0] <> cornerLetters[3] && cornerLetters[1] <> cornerLetters[2] then
                    1
                else
                    0
            else
                0
        else
            0
    ] |> List.sum
    

let test =
    let day = 4
    let test1 = """
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"""
    calc1 test1 |> should equal 18

    let input = readDailyInput day
    calc1 input |> should equal 2662

    calc2 test1 |> should equal 9
    calc2 input |> should equal 2034
    
test    
