#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"
#load "AdventArray.fs"

open System
open System.Diagnostics
open System.IO
open System.Text.Json.Serialization.Metadata
open AdventArray
open Utility
open FsUnit.Xunit

type BlockType =
    | Free
    | File of int
type Segment = { Type: BlockType; Len: int }

let readLengths (input:string) = input.ToCharArray() |> Array.map (string >> int)

let parse (input:string) =
    let lengths = readLengths input
    let len = Array.sum lengths
    let arr = Array.create len -1
    let mutable pos = 0
    for i in 0..(lengths.Length - 1) do
        let value = if i % 2 = 0 then i / 2 else -1
        for j in 0..(lengths[i] - 1) do
            arr[pos + j] <- value
        pos <- pos + lengths[i]
    arr

let parseSegments (input:string) =
    let lengths = readLengths input
    let mutable pos = 0
    lengths |> List.ofArray |> List.mapi (fun i b -> { Len=b; Type=if i % 2 = 0 then File (i / 2) else Free })
    
let defrag a =
    let a = Array.copy a
    let mutable left = 0
    let mutable right = Array.length a-1
    while left < right do
        let v = a[right]
        if v <> -1 then
            while left < right && a[left] <> -1 do
                left <- left + 1
            if left < right then
                a[left] <- v
                a[right] <- -1
        right <- right - 1
    a

let checksum a =
    a |> Seq.ofArray |> Seq.mapi (fun i v -> if v <> -1 then int64 (i * v) else 0L) |> Seq.sum

let calc1 = parse >> defrag >> checksum

let defrag2 segments =
    let findFree segs len leftOf =
        segs |> List.indexed |> List.filter (fun is -> fst is < leftOf) |> List.tryFindIndex (fun is -> let f = snd is in f.Type.IsFree && f.Len >= len)
    let rec loop (segs:Segment list) pos =
        if pos = -1 then
            segs
        else
            match segs[pos] with
            | { Len=len; Type=File id } ->
                match findFree segs len pos with
                | Some f ->
                    let freeSegmentAfter = { segs[f] with Len = segs[f].Len - len }
                    let fileSegment = { Len=len; Type=File id }
                    let segs = segs |> List.removeAt pos |> List.insertAt pos { Len=len; Type=Free} |> List.removeAt f |> List.insertAt f freeSegmentAfter |> List.insertAt f fileSegment
                    loop segs (pos - 1)
                | None -> loop segs (pos - 1)
            | { Len=_; Type=Free } -> loop segs (pos - 1)
    loop segments (segments.Length - 1) |> List.filter (fun s -> s.Len > 0)

let checksum2 (segments:Segment list) =
    let mutable pos = 0
    let mutable checksum = 0L
    for i in [0..(segments.Length - 1)] do
        let s = segments[i]
        for j in [0..(s.Len - 1)] do
            match s.Type with
            | File f -> checksum <- checksum + int64 (f * pos)
            | Free -> ()
            pos <- pos + 1
    checksum
    
let calc2 = parseSegments >> defrag2 >> checksum2

let test =
    let sw = Stopwatch.StartNew()
    let day = 9
    
    let test0 = "12345"
    (parse test0) |> should equal (Array.ofList [0;-1;-1;1;1;1;-1;-1;-1;-1;2;2;2;2;2])
    let test1 = "2333133121414131402"
    (parse test1) |> should equal (Array.ofList [0;0;-1;-1;-1;1;1;1;-1;-1;-1;2;-1;-1;-1;3;3;3;-1;4;4;-1;5;5;5;5;-1;6;6;6;6;-1;7;7;7;-1;8;8;8;8;9;9])
    defrag (parse test0) |> should equal (Array.ofList [0;2;2;1;1;1;2;2;2;-1;-1;-1;-1;-1;-1])

    calc1 test1 |> should equal 1928L
    
    let input = (readDailyInput day).Trim()

    calc1 input |> should equal 6415184586041L
    
    test0 |> parseSegments |> should equal [{Len=1; Type=File 0};{Len=2; Type=Free};{Len=3; Type=File 1};{Len=4; Type=Free};{Len=5; Type=File 2}]
    test1 |> parseSegments |> defrag2 |> checksum2 |> should equal 2858L

    calc2 input |> should equal 6436819084274L
    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
