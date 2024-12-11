#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"
#load "AdventArray.fs"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open AdventArray
open Utility
open FsUnit.Xunit

let parse = splitSpace >> List.map int64

let blink1stone stone =
    match stone with
    | 0L -> [1L]
    | n when (string n).Length % 2 = 0 -> let s = string n in let l = s.Length in [int64 s[..l/2-1]; int64 s[l/2..]]
    | _ -> [2024L * stone]

let mutable cache = Map.empty
let rec blink2 n stone =
    if n = 0 then
        1L
    else
        let key = (stone, n)
        match cache |> Map.tryFind key with
        | Some v -> v
        | None ->
            let vv = blink1stone stone |> List.sumBy (blink2 (n - 1))
            cache <- cache |> Map.add key vv
            vv

let calc n input =
    input |> List.sumBy (blink2 n)

let calc1 = calc 25 

let calc2 = calc 75

let test =
    let sw = Stopwatch.StartNew()
    let day = 11
    
    let test0 = parse "125 17"
    // blink test0 |> should equal [253000L; 1L; 7L]
    // blink 2 test0 |> should equal [253L; 0L; 2024L; 14168L]
    // blink 3 test0 |> should equal [512072L; 1L; 20L; 24L; 28676032L]
    // blink 4 test0 |> should equal [512L; 72L; 2024L; 2L; 0L; 2L; 4L; 2867L; 6032L]
    calc 1 test0 |> should equal 3L
    calc 2 test0 |> should equal 4L
    calc 3 test0 |> should equal 5L
    calc 4 test0 |> should equal 9L
    calc 5 test0 |> should equal 13L
    calc 6 test0 |> should equal 22L
    
    calc1 test0 |> should equal 55312L

    let input = parse (readDailyInput day)
    
    calc2 input |> should equal 244782991106220L
    
    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
