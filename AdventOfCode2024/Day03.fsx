#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"

open System.Text.RegularExpressions
open Utility
open FsUnit.Xunit

type Token =
    | Mul of int * int
    | Do
    | Dont

let parse (inputText:string) =
    let re = Regex(@"(?<dont>don't\(\))|(?<do>do\(\))|(?<mul>mul\((\d{1,3}),(\d{1,3})\))")
    [
        for m in re.Matches(inputText) do
            if m.Groups["mul"].Success then
                Mul (int m.Groups[1].Value, int m.Groups[2].Value)
            elif m.Groups["do"].Success then
                Do
            elif m.Groups["dont"].Success then
                Dont
            else
                failwithf $"Unexpected match {m.Value} @ {m.Index}"
    ]

let eval1 = function
    | Mul (a, b) -> a*b
    | Do -> 0
    | Dont -> 0

let calc1 = parse >> List.sumBy eval1

let folder2 (state, acc) t =
    match t with
    | Do -> (1, acc)
    | Dont -> (0, acc)
    | Mul(a, b) -> (state, acc + state * a * b)
    
let calc2 = parse >> List.fold folder2 (1, 0) >> snd

let test =
    let day = 3
    let test1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    calc1 test1 |> should equal 161

    let input = readDailyInput day
    calc1 input |> should equal 173731097
    
    let test2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    calc2 test2 |> should equal 48
    calc2 input |> should equal 93729253
    
test    
