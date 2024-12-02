#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"

open Utility
open System
open FsUnit.Xunit

type Report = int list
type Input = Report list

let parse (inputText:string) =
    inputText |> getLines |> List.map (splitSpace >> List.map int)

let isGraduallyIncreasing  levels =
    levels |> List.pairwise |> List.forall (fun (l1, l2) -> 1 <= (l2 - l1) && (l2 - l1) <= 3)

let isSafe levels =
    (isGraduallyIncreasing levels) || (isGraduallyIncreasing (List.rev levels))

let calc1 (input:Input) =
    input |> List.filter isSafe |> List.length

let calc2 (input:Input) =
    let reducedReports levels =
        [0..(List.length levels - 1)] |> List.map (fun i -> (List.removeAt i levels))
    
    let isAnySafe levels =
        reducedReports levels |> List.exists isSafe  

    input |> List.filter isAnySafe |> List.length

let test =
    let day = 2
    let input1 = parse "
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"
    calc1 input1 |> should equal 2

    let input = parse (readDailyInput day)
    calc1 input |> should equal 549

    calc2 input1 |> should equal 4
    calc2 input |> should equal 589
    
test    
