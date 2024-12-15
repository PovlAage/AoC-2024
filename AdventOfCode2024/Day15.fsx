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
open Utility
open FsUnit.Xunit

type Point = Point of (int * int)
type Vec = Vec of (int * int)
let addvec (Point p) (Vec v) = Point (addvec p v)
let scalevec (Vec v) c = Vec (c * fst v, c * snd v)
let directions4 = directions4 |> List.map Vec
let m2v c =
    match c with
    | '^' -> directions4[0]
    | '>' -> directions4[1]
    | 'v' -> directions4[2]
    | '<' -> directions4[3]
    | _ -> failwith $"Invalid {c}"
    
let parse input =
    let arr, moves =
        match parseBlocks input with
        | [mapLines; moveLines] -> parseArray2D (String.Join("\n", mapLines)) 0 ' ', String.Join("", moveLines)
        | _ -> failwith "Invalid block"
    let pos = find '@' arr
    arr[fst pos, snd pos] <- '.'
    (Point pos), arr, moves

let dumpArray enable arr p i m=
    if enable then
        let maxx, maxy = AdventArray.maxes arr 0
        for y in [0..maxy] do
            for x in [0..maxx] do
                let c = if (Point (x, y) = p) then m else arr[x, y]
                printf $"{c}"
            printf "\n"
        printfn $"{i}:{m}"
        
let widen input =
    let Point pos, arr, moves = input
    let arr2 = Array2D.init (2 * Array2D.length1 arr) (Array2D.length2 arr) (fun x y -> if arr[x/2, y] = 'O' then (if x % 2 = 0 then '[' else ']') else arr[x/2, y])
    let pos2 = Point (fst pos * 2, snd pos)
    pos2, arr2, moves

let move (arr:char array2d) pos m =
    let v = m2v m
    let moveBlock (Point src) (Point dest) =
        assert (arr[fst dest, snd dest] = '.')
        // printfn $"moveBlock {src}->{dest}"
        arr[fst dest, snd dest] <- arr[fst src, snd src]
        arr[fst src, snd src] <- '.'
    let rec loop prev =
        let prevAndCurrent = prev |> List.map (fun p -> p, addvec p v)
        let get (Point (x, y)) = arr[x, y]
        if prevAndCurrent |> List.exists (fun (_, p) -> get p = '#') then
            // hit a wall, no move
            false
        elif prevAndCurrent |> List.forall (fun (_, p) -> get p = '.') then
            // found free space, move prev to current and return
            for prev, current in prevAndCurrent do moveBlock prev current
            true
        else
            let boxesInCurrentRow = prevAndCurrent |> List.map snd |> List.filter (fun p -> get p <> '.') // know not #
            // add other part of boxes, if necessary
            let boxesInCurrentRow =
                match m with
                | '^' | 'v' ->
                    let otherHalf (Point (x, y)) =
                        match arr[x - 1, y], arr[x, y], arr[x + 1, y] with
                        | _, '[', ']' -> Some (Point (x + 1, y))
                        | '[', ']', _ -> Some (Point (x - 1, y))
                        | _ -> None
                    let additional = boxesInCurrentRow |> Seq.choose otherHalf |> Set.ofSeq
                    (Set.union (Set.ofSeq boxesInCurrentRow) additional) |> Seq.toList
                | _ -> boxesInCurrentRow

            // let next level decide
            if loop boxesInCurrentRow then
                // could move, so now there is space here
                for prev, current in prevAndCurrent do moveBlock prev current
                true
            else
                // could not move, exit
                false
        
    let mutable charCountsBefore = Map.ofList [('#', 0); ('[', 0); (']', 0); ('O', 0)]
    arr |> Array2D.iter (fun c -> if c <> '.' then charCountsBefore <- charCountsBefore.Add(c, charCountsBefore[c] + 1)) |> ignore
    let moved = loop ([pos])
    let mutable charCountsAfter = Map.ofList [('#', 0); ('[', 0); (']', 0); ('O', 0)]
    arr |> Array2D.iter (fun c -> if c <> '.' then charCountsAfter <- charCountsAfter.Add(c, charCountsAfter[c] + 1)) |> ignore
    if charCountsAfter <> charCountsBefore then failwithf $"{charCountsBefore} <> {charCountsAfter}"
    if moved then addvec pos v else pos
    
let calc input=
    let pos, arr, moves = input
    let dump arr p i m = dumpArray false arr p i m
    let pos = [1..(String.length moves)] |> List.map (fun i -> i, moves[i-1]) |> List.fold (fun p (i, m) -> dump arr p i m; move arr p m) pos
    let mutable acc = 0
    let maxx, maxy = AdventArray.maxes arr 0
    for x in 0..maxx do
        for y in 0..maxy do
            if arr[x, y] = 'O' || arr[x, y] = '[' then acc <- acc + 100 * y + x
    dumpArray true arr pos -1 '@'
    acc                            

let test =
    let sw = Stopwatch.StartNew()
    let day = 15

    let test0 = """
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
"""  
    calc (parse test0) |> should equal 2028
    
    let test1 = """
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
"""
    calc (parse test1) |> should equal 10092
    
    let input = readDailyInput day |> parse
    calc input |> should equal 1515788
    let test2small = """
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^
"""

    // this is step 312
    let testSpecific1 = """
####################
##[]..[]......[][]##
##[]...........[].##
##............[][]##
##..........[].[].##
##..##[]..[].[]...##
##...[]...[]..[]..##
##.....[]..[].[][]##
##........[]......##
####################
"""
    let arr = AdventArray.parseArray2D testSpecific1 0 ' '
    let p = Point (13, 3)
    let p2 = (move arr p 'v') 
    dumpArray true arr p2 -1 'v'
    p2 |> should equal (Point (13, 4))

    let testSpecific2 = """
############
##........##
##...[]...##
##..[][]..##
##.[]..[].##
###..##..###
############
"""
    let arr = AdventArray.parseArray2D testSpecific2 0 ' '
    let p = Point (5, 1)
    let p2 = (move arr p 'v')
    dumpArray true arr p2 -1 'v'
    p2 |> should equal (Point (5, 2))
    let p = Point (6, 1)
    let p2 = (move arr p 'v')
    dumpArray true arr p2 -1 'v'
    p2 |> should equal (Point (6, 2))
    
    let testSpecific3 = """
#######
#.....#
#.OO@.#
#.....#
#######

<<
"""
    calc (testSpecific3 |> parse |> widen) |> should equal 406

    let testSpecific4 = """
#######
#.....#
#.O#..#
#..O@.#
#.....#
#######

<v<<^
"""
    calc (testSpecific4 |> parse |> widen) |> should equal 509

    let testSpecific5 = """
######
#....#
#..#.#
#....#
#.O..#
#.OO@#
#.O..#
#....#
######

<vv<<^^^
"""
    calc (testSpecific5 |> parse |> widen) |> should equal 1216
    
    let testSpecific6 = """
########
#......#
#OO....#
#.O....#
#.O....#
##O....#
#O..O@.#
#......#
########

<^^<<>^^^<v
"""
    calc (testSpecific6 |> parse |> widen) |> should equal 2827
    
    let testSpecific7 = """
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
"""
    calc (testSpecific7 |> parse |> widen) |> should equal 1751
    
    let testSpecific8 = """
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^
"""
    calc (testSpecific8 |> parse |> widen) |> should equal 618
    
    calc (test2small |> parse |> widen) |> ignore

    calc (test0 |> parse |> widen) |> ignore

    calc (test1 |> parse |> widen) |> should equal 9021
    calc (widen input) |> should equal 1516544
    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
