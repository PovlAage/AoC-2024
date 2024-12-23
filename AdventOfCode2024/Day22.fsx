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
open FsUnit.CustomMatchers
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Quotations
open Utility
open FsUnit.Xunit

let parse= parseLines >> List.map int

let nextSecret n =
    // <<< 6 og xor - 
    // >>> 5 og xor - 0-4 xor 5-9
    // <<< 11 og xor - ændrer ikke på nederste 11 bits
    let mix secret value = secret ^^^ value
    let prune n = n &&& (16777216 - 1)
    let n = (n <<< 6) |> mix n |> prune
    let n = (n >>> 5) |> mix n |> prune
    let n = (n <<< 11) |> mix n |> prune
    n

let allSecrets = (Seq.init 16777216 id) |> Seq.map nextSecret |> Array.ofSeq

let nextSecretK k n =
    [1..k] |> List.fold (fun m _ -> allSecrets[m]) n

let getPrices k n =
    [1..k] |> List.scan (fun x _ -> let n, p = x in let s = allSecrets[n] in (s, int (s % 10))) (n, int (n % 10))

let encodeChanges (k0, k1, k2, k3) =
    let c0, c1, c2, c3 = k0 + 9, k1 + 9, k2 + 9, k3 + 9
    c0 + (c1 <<< 5) + (c2 <<< 10) + (c3 <<< 15)

let encodeChangesList keyList =
    match keyList with
    | [k0; k1; k2; k3] -> encodeChanges (k0, k1, k2, k3)
    | _ -> failwith $"{keyList}"

let pricesToChanges prices =
    prices |> List.windowed 5 |> List.map (List.pairwise >> List.map (fun (x1, x2) -> x2 - x1) >> encodeChangesList) |> Array.ofList

let bestKey k ns =
     let prices = ns |> List.map (getPrices k >> List.map snd)
     let changes = prices |> List.map pricesToChanges
     let preCompute (priceList, changeArray) =
         Seq.zip (priceList |> Seq.skip 4) (changeArray) |> Seq.fold (fun m x -> let p, c = x in if not (m |> Map.containsKey c) then m |> Map.add c p else m) Map.empty 
     let keyToPrice = List.zip prices changes |> List.map preCompute
     let getTotalSalesPrice key =
         keyToPrice |> List.choose (Map.tryFind key) |> List.sum
         
     let existingKeys = Set.unionMany (keyToPrice |> Seq.map _.Keys |> Seq.map Set.ofSeq)
     existingKeys |> Seq.map getTotalSalesPrice |> Seq.max

let calc1 nums =
    nums |> List.sumBy (nextSecretK 2000 >> int64)

let calc2 nums =
    bestKey 2000 nums

let test =
    let sw = Stopwatch.StartNew()
    let day = 22

    let test0 = [123;15887950;16495136;527345;704524;1553684;12683156;11100544;12249484;7753432;5908254]
    allSecrets[123] |> should equal 15887950
    allSecrets[15887950] |> should equal 16495136
    [for n0, n1 in (List.pairwise test0) do (allSecrets[n0] |> should equal n1)] |> ignore
    nextSecretK 2000 1 |> should equal 8685429
    let test1 = parse """
1
10
100
2024
"""

    calc1 test1 |> should equal 37327623L

    let input = parse (readDailyInput day)
    
    calc1 input |> should equal 19150344884L

    (getPrices 10 123)[0] |> should equal (123, 3)
    (getPrices 10 123)[1] |> should equal (15887950, 0)
    (getPrices 10 123)[2] |> should equal (16495136, 6)
    (getPrices 10 123)[3] |> should equal (527345, 5)
    (getPrices 10 123)[4] |> should equal (704524, 4)
    (getPrices 10 123)[5] |> should equal (1553684, 4)
    (getPrices 10 123)[6] |> should equal (12683156, 6)
    (getPrices 10 123)[7] |> should equal (11100544, 4)
    (getPrices 10 123)[8] |> should equal (12249484, 4)
    (getPrices 10 123)[9] |> should equal (7753432, 2)
    
    (calc2 [1;2;3;2024]) |> should equal 23

    (calc2 input) |> should equal 2121

    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"

test
printfn $"Done"
