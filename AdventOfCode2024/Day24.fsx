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
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Quotations
open Utility
open FsUnit.Xunit

type WireId = string
type Wire = Wire of WireId * int
type Gate =
    | AND of WireId * WireId
    | OR of WireId * WireId
    | XOR of WireId * WireId
    | VAL of int
type GateInOut = Gate * WireId
type Circuit = Map<WireId, Gate>
let parseWire (s:string) =
    let w, v = split2 ':' s
    Wire (w, int v)
    
let parseGate (s:string) =
    match splitSpace s with
    | [in1; gate; in2; _; out] ->
        match gate with
        | "AND" -> AND (in1, in2), out
        | "OR" -> OR (in1, in2), out
        | "XOR" -> XOR (in1, in2), out
        | _ -> failwith $"{gate}"
    | _ -> failwith $"{s}"

let connect inputWires gates =
    let rec loop gates circuit =
        match gates with
        | [] -> circuit
        | gate :: rest -> let gate, out = gate in loop rest (circuit |> Map.add out gate )
    
    let initialCircuit = inputWires |> Seq.map (fun (Wire (w, v)) -> (w, VAL v)) |> Map.ofSeq
    loop gates initialCircuit

let parse input =
    let inputWires, gates = parseBlocks2 input
    let inputWires = inputWires |> List.map parseWire
    let gates = gates |> List.map parseGate
    inputWires, gates

let wireName prefix bitNo =
    if bitNo > 9 then $"{prefix}{bitNo}" else $"{prefix}0{bitNo}"

let eval circuit wireId=
    let mutable values = Map.empty
    let rec eval wireId =
        match values |> Map.tryFind wireId with
        | Some v -> v
        | None ->
            if not (circuit |> Map.containsKey wireId) then
                for k in circuit.Keys |> Seq.sort do
                    printfn $"{k}"
                failwith $"No wire {wireId}"
            let gate = circuit |> Map.find wireId
            let v = match gate with
                    | AND (w1, w2) -> eval w1 &&& eval w2
                    | OR (w1, w2) -> eval w1 ||| eval w2
                    | XOR (w1, w2) -> eval w1 ^^^ eval w2
                    | VAL v -> v
            values <- values.Add(wireId, v)
            v
    eval wireId

let calc1 (circuit:Circuit) =
    let outputs = circuit.Keys |> Seq.filter _.StartsWith('z') |> List.ofSeq |> List.sort
    outputs |> List.indexed |> List.sumBy (fun (i, w) -> (int64 (eval circuit w)) <<< i)

let calc2 circuit topBit =
    let rec verifyLoop circuit bit (carry:WireId option) =
        let getDescendants w =
            let isDescendant = function
            | AND (w1, w2) when w1 = w || w2 = w -> true
            | OR (w1, w2) when w1 = w || w2 = w -> true
            | XOR (w1, w2) when w1 = w || w2 = w -> true
            | _ -> false
            circuit |> Map.filter (fun k v -> isDescendant v) |> Map.toSeq |> Set.ofSeq
            
        let getInputs = function
            | AND (w1, w2) -> [w1;w2]
            | OR (w1, w2) -> [w1;w2]
            | XOR (w1, w2) -> [w1;w2]
            | VAL _ -> []

        printfn $"Bit {bit}:"
        let x = wireName "x" bit
        let y = wireName "y" bit
        let z = wireName "z" bit
        let dx = getDescendants x
        let dy = getDescendants y
        if dx <> dy then printfn $"dx<>dy: {dx}, {dy}"
        let xor0 = dx |> Set.toSeq |> Seq.find (snd >> _.IsXOR)
        let and0 = dx |> Set.toSeq |> Seq.find (snd >> _.IsAND)
        if dx.Count <> 2 then printfn $"|dx|<>2: {dx}"

        if bit = 0 then
            let nextCarry = fst and0
            verifyLoop circuit (bit + 1) (Some nextCarry)
        else
            let out = circuit |> Map.find z
            if not out.IsXOR then
                // output was swapped. Find it from carry instead
                let carryDescendants = getDescendants carry.Value
                let realOutput = carryDescendants |> Set.toSeq |> Seq.filter (snd >> _.IsXOR) |> Seq.exactlyOne
                printfn $"out is not xor: {out}, realOutput is {realOutput}"
                let swap = (fst realOutput, z)
                Some swap
            else
                let outInputs = getInputs out |> Set.ofSeq
                if not (outInputs |> Set.contains (fst xor0)) then
                    printfn $"xor0 not input to out: xor0={xor0}, out={out}, carry={carry}"
                    if not (outInputs.Contains(carry.Value)) then failwith $"Carry {carry} should be in outInputs {outInputs}"
                    let toSwap = outInputs |> Set.filter (fun x -> x <> carry.Value) |> Seq.exactlyOne
                    let swap = (toSwap, fst xor0)
                    Some swap
                else
                    let xor1 = (getDescendants (fst xor0)) |> Set.toSeq |> Seq.find (snd >> _.IsXOR)
                    let and1 = (getDescendants (fst xor0)) |> Set.toSeq |> Seq.find (snd >> _.IsAND)
                    let or0 = (getDescendants (fst and0)) |> Set.toSeq |> Seq.find (snd >> _.IsOR)
                    
                    if z <> fst xor1 then
                        printfn $"xor1 should go to output {z} but xor1: {xor1}"
                        let swap = (z, fst xor1)
                        Some swap
                    else
                        let inputXor1 = (snd xor1) |> getInputs |> Set.ofSeq
                        let inputAnd1 = (snd and1) |> getInputs |> Set.ofSeq

                        let carry = carry.Value
                        if not (inputXor1 |> Set.contains carry) then printfn $"Carry {carry} not in inputXor1: {inputXor1}"
                        if not (inputAnd1 |> Set.contains carry) then printfn $"Carry {carry} not in inputAnd1: {inputAnd1}"

                        let inputOr0 = (snd or0) |> getInputs |> Set.ofSeq
                        if not (Set.isSubset (Set [fst and0; fst and1]) inputOr0) then printfn $"Or must have and0 and and1 as input, but inputOr0: {inputOr0}"

                        if bit < topBit then
                            let nextCarry = fst or0
                            verifyLoop circuit (bit + 1) (Some nextCarry)
                        else
                            None

    let rec swapLoop swaps =
        let oneSwap circuit (k0, k1) =
            printfn $"Swapping {k0}-{k1}"
            let v0 = circuit |> Map.find k0
            let v1 = circuit |> Map.find k1
            circuit |> Map.add k0 v1 |> Map.add k1 v0
        let swappedCircuit = swaps |> List.fold oneSwap circuit
        match verifyLoop swappedCircuit 0 None with
        | Some swap -> swapLoop (swap :: swaps)
        | None ->
            let swappedWires = swaps |> List.collect (fun (x0, x1) -> [x0;x1])
            String.Join(',', swappedWires |> List.sort)

    swapLoop []
    
let test =
    let sw = Stopwatch.StartNew()
    let day = 24

    let wires1, gates1 = parse """
x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
"""

    calc1 (connect wires1 gates1) |> should equal 4L

    let wires2, gates2 = parse """
x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj
"""
    calc1 (connect wires2 gates2) |> should equal 2024L

    let wires, gates = parse (readDailyInput day)
    calc1 (connect wires gates) |> should equal 59336987801432L

    calc2 (connect wires gates) 44 |> should equal "ctg,dmh,dvq,rpb,rpv,z11,z31,z38"

    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"

test
printfn $"Done"
