#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"
#load "AdventArray.fs"

open AdventArray
open Utility
open FsUnit.Xunit

let encodeDirections = [1;2;4;8]
let walk arr =
    let pos = find '^' arr
    set (char encodeDirections[0]) pos arr
    let rec loop pos dirIndex =
        let nextPos = add pos directions4[dirIndex % 4]
        let encoded = encodeDirections[dirIndex % 4]
        match arr |> AdventArray.item nextPos with
        | '.' -> set (char encoded) nextPos arr ; loop nextPos dirIndex
        | c when char 0 <= c && c <= char 15 && (int encoded &&& int c <> 0) -> -1  // loop 
        | c when char 0 <= c && c <= char 15 -> set (char (int c ||| encoded)) nextPos arr ; loop nextPos dirIndex
        | '#' -> loop pos (dirIndex + 1)
        | ' ' -> arr |> Seq.cast<char> |> Seq.filter (fun cc -> char 0 <= cc && cc <= char 15) |> Seq.length
        | c -> failwithf $"Unexpected '{c}'"
    let dirIndex = 0
    assert (directions4[dirIndex] = (0, -1))
    loop pos dirIndex

let calc1 input =
    let arr = parseArray2D input 1 ' '
    walk arr

let calc2 input =
    let arrClean = parseArray2D input 1 ' '
    let arr = Array2D.copy arrClean
    let isLoop x y =
        Array2D.blit arrClean 0 0 arr 0 0 (Array2D.length1 arr) (Array2D.length2 arr)
        match arr[x,y] with
        | '^' | ' ' -> false
        | _ -> arr[x,y] <- '#' ; (walk arr) = -1

    [for x in [1..Array2D.length1 arrClean - 2] do for y in [1..Array2D.length2 arrClean - 2] do isLoop x y] |> List.filter id |> List.length

let test =    
    let day = 6
    let test1 = """
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"""
    calc1 test1 |> should equal 41

    let input = readDailyInput day
    calc1 input |> should equal 5531

    calc2 test1 |> should equal 6
    calc2 input |> should equal 2165
    printfn $"Day {day} done"
    
test
printfn $"Done"
