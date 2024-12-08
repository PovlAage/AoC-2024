#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"
#load "AdventArray.fs"

open System.Diagnostics
open AdventArray
open Utility
open FsUnit.Xunit

type Operator = Add | Multiply | Concat
type Line = int64 * int64 list 

let parse input =
    let parseLine (line:string) =
        let expected, remain = split2 ':' line
        let values = splitSpace remain
        int64 expected, values |> List.map int64
    input |> parseLines |> List.map parseLine

let eval (a:int64) (b:int64) = function 
    | Add -> a + b
    | Multiply -> a * b
    | Concat -> int64 (string a + string b)

[<TailCall>]
let rec isValidLoop (ops:Operator list) (expected:int64) (values:int64 list) i acc =
    if acc > expected then
        false
    elif i = values.Length then
        acc = expected
    else
        ops |> Seq.exists (fun op -> isValidLoop ops expected values (i + 1) (eval acc values[i] op))

let isValid ops ((expected, values):Line) = isValidLoop ops expected values 1 (List.head values)

let calc1 input =
    let input = parse input
    input |> List.filter (isValid [Add; Multiply]) |> List.sumBy fst

let calc2 input =
    let input = parse input
    input |> List.filter (isValid [Add; Multiply; Concat]) |> List.sumBy fst

let test =
    let sw = Stopwatch.StartNew()
    let day = 7
    
    isValid [Add] (5L, [2L; 3L]) |> should equal true
    isValid [Add] (6L, [2L; 3L]) |> should equal false
    isValid [Multiply] (5L, [2L; 3L]) |> should equal false

    isValid [Add;Multiply] (172L, [9;8;1;26;4;73] |> List.map int64) |> should equal false
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
