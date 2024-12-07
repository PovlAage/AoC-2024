#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"
#load "AdventArray.fs"

open System.Diagnostics
open AdventArray
open Utility
open FsUnit.Xunit

type Operator = Add | Multiply | Concatenation
type Line = int64 * int64 list 

let parse input =
    let parseLine (line:string) =
        let expected, remain = split2 ':' line
        let values = splitSpace remain
        int64 expected, values |> List.map int64
    input |> parseLines |> List.map parseLine

let eval (a:int64) (b:int64) op =
    let res = match op with
                | Add -> a + b
                | Multiply -> a * b
                | Concatenation -> int64 (string a + string b)
    // printfn $"{op} {a} {b} = {res}";
    res

[<TailCall>]
let rec isValidImpl (ops:Operator list) (expected:int64) (values:int64 list) i acc =
    if acc > expected then
        0L
    elif i = values.Length then
        if acc = expected then
            expected
        else
            0L
    else
        ops |> List.map (fun op -> isValidImpl ops expected values (i + 1) (eval acc values[i] op)) |> List.max

let isValid ops ((expected, values):Line) = isValidImpl ops expected values 1 (List.head values)

let calc1 input =
    let input = parse input
    input |> List.sumBy (isValid [Add; Multiply])

let calc2 input =
    let input = parse input
    input |> List.sumBy (isValid [Add; Multiply; Concatenation])

let test =
    let sw = Stopwatch.StartNew()
    let day = 7
    
    isValid [Add] (5L, [2L; 3L]) |> should equal 5L
    isValid [Add] (6L, [2L; 3L]) |> should equal 0L
    isValid [Multiply] (5L, [2L; 3L]) |> should equal 0L

    isValid [Add;Multiply] (172L, [9;8;1;26;4;73] |> List.map int64) |> should equal 0L
    let test1 = """
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"""
    calc1 test1 |> should equal 3749L
    
    let input = readDailyInput day
    calc1 input |> should equal 7579994664753L
    
    calc2 test1 |> should equal 11387L
    calc2 input |> should equal 438027111276610L
    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
