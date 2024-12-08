#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"
#load "AdventArray.fs"

open System.Diagnostics
open AdventArray
open Utility
open FsUnit.Xunit

let countVisited arr =
    let pos = find '^' arr
    set 'X' pos arr
    let rec loop pos dirIndex =
        let nextPos = addvec pos directions4[dirIndex % 4]
        match arr |> AdventArray.item nextPos with
        | '.' | 'X' -> set 'X' nextPos arr ; loop nextPos dirIndex
        | '#' -> loop pos (dirIndex + 1)
        | ' ' -> arr |> Seq.cast<char> |> Seq.filter (fun cc -> cc = 'X') |> Seq.length
        | c -> failwithf $"Unexpected '{c}'"
    let dirIndex = 0
    assert (directions4[dirIndex] = (0, -1))
    loop pos dirIndex

let calc1 input =
    let arr = parseArray2D input 1 ' '
    countVisited arr

let isLoop arr pos extraObstacle =
    match arr |> item extraObstacle with
    | '^' -> false // illegal
    | '#' -> false // already know that this is not a loop
    | '.' -> 
        set '#' extraObstacle arr
        let rec loop pos dirIndex (visited:Set<int*int>) =
            let nextPos = addvec pos directions4[dirIndex]
            match (arr |> AdventArray.item nextPos, dirIndex) with
            | '.', _ -> loop nextPos dirIndex visited
            | '#', 0 when Set.contains pos visited -> true
            | '#', 0 -> loop pos ((dirIndex + 1) % 4) (Set.add pos visited)
            | '#', _ -> loop pos ((dirIndex + 1) % 4) visited
            | ' ', _ -> false
            | c -> failwithf $"Unexpected '{c}'"
        let dirIndex = 0
        assert (directions4[dirIndex] = (0, -1))
        let res = loop pos dirIndex Set.empty
        set '.' extraObstacle arr
        res
    | c -> failwithf $"unexpected {c}"

let calc2 input =
    let arr = parseArray2D input 1 ' '
    let pos = arr |> find '^'
    arr[fst pos, snd pos] <- '.'
    [for x in [1..Array2D.length1 arr - 2] do for y in [1..Array2D.length2 arr - 2] do isLoop arr pos (x, y)] |> List.filter id |> List.length

let test =
    let sw = Stopwatch.StartNew()
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
    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
