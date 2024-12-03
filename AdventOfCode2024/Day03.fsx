#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"

open System.Text.RegularExpressions
open Utility
open FsUnit.Xunit

type Report = int list
type Input = Report list
type Mul = Mul of int * int

let parse (inputText:string) =
    let re = Regex("""mul\((\d{1,3}),(\d{1,3})\)""")
    [for m in re.Matches(inputText) do Mul (int m.Groups[1].Value, int m.Groups[2].Value)]

let replace (inputText:string) =
    let re = Regex(@"don't\(\).*?(do\(\)|$)", RegexOptions.Singleline)
    let replaced = re.Replace(inputText, "")
    if replaced.Contains("don't") then
        let index = replaced.IndexOf("don't")
        printfn $"{replaced.Substring(index, 100)}"
        failwithf $"""don't at {index}""" 
    replaced

let calc1 = parse >> List.sumBy (fun (Mul (a, b)) -> a * b)

let calc2 = replace >> calc1

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
