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
open Utility
open FsUnit.Xunit

type Opcode =
    | ADV = 0
    | BXL = 1
    | BST = 2
    | JNZ = 3
    | BXC = 4
    | OUT = 5
    | BDV = 6
    | CDV = 7
type RegisterId = A | B | C
type State = { IP: int; Registers: Map<RegisterId, int64>; Output: int list }

let parse input =
    let stateBlock, programBlock = parseBlocks2 input
    let r (line:string) = int (line.Split(':')[1]) 
    let a, b, c = r stateBlock[0], r stateBlock[1], r stateBlock[2]
    let program = (programBlock[0].Split(':')[1]).Split(',') |> Array.toList |> List.map int
    let state = { IP=0; Registers = Map.ofList [(RegisterId.A, a); (RegisterId.B, b); (RegisterId.C, c)]; Output = [] }
    state, program

let getComboValue opCode (state:State) =
    match opCode with
    | 0 | 1 | 2 | 3 -> int64 opCode
    | 4 -> state.Registers.Item RegisterId.A
    | 5 -> state.Registers.Item RegisterId.B
    | 6 -> state.Registers.Item RegisterId.C
    | _ -> failwith $"Invalid combo operand {opCode}"

let interpretStep (program:int list) (state:State) =
    let opcode = enum<Opcode> program[state.IP]
    let operand = program[state.IP + 1]
    let getReg r = state.Registers.Item r
    let writeRegAndInc r v = { state with IP = state.IP + 2; Registers = state.Registers.Add(r, v) }
    let xdv r =
        let numerator = getReg RegisterId.A
        let power2 = getComboValue operand state
        assert (0L <= power2 && power2 <= 31L)
        let denominator = (int64 0b1) <<< int power2
        let result = numerator / denominator // truncate
        writeRegAndInc r result
    let mod8 v = int (v % 8L)
    match opcode with
    | Opcode.ADV ->
        xdv RegisterId.A
    | Opcode.BXL ->
        let result = (getReg RegisterId.B) ^^^ operand
        writeRegAndInc RegisterId.B result
    | Opcode.BST ->
        let result = mod8 (getComboValue operand state)
        writeRegAndInc RegisterId.B result
    | Opcode.JNZ ->
        { state with IP = if getReg RegisterId.A <> 0 then operand else state.IP + 2 }
    | Opcode.BXC ->
        let result = (getReg RegisterId.B) ^^^ (getReg RegisterId.C)
        writeRegAndInc RegisterId.B result
    | Opcode.OUT ->
        { state with IP = state.IP + 2; Output = mod8 (getComboValue operand state) :: state.Output }
    | Opcode.BDV ->
        xdv RegisterId.B
    | Opcode.CDV ->
        xdv RegisterId.C
    | _ -> failwith $"Invalid opcode {opcode}"
        
let run (state:State) (program:int list) =
    let mutable state = state
    while 0 <= state.IP && state.IP < program.Length do
        state <- interpretStep program state
    state

let calc1 (state, program)=
    let endState = run state program
    String.Join(',', endState.Output |> List.rev)

// output only depends on lower 11 bits of input a, so cache all values
let outputBits = 0b1 <<< 12
let outputBits64 = int64 outputBits
let calcOutput a =
    assert (a < outputBits)
    let b = (a % 8) ^^^ 3 // 3 bits
    let c = a >>> b // a-b bits
    let b = b ^^^ c // 3 bits
    let b = b ^^^ 5
    b % 8
let outputcache = Array.init outputBits calcOutput

let calc2 (program:int list) =
    let rec loop candidates k =
        let isCorrectOutput x = outputcache[int (x % outputBits64)] = program[k]
        let getNextCandidates c = [0..7] |> List.map (fun b3 -> (c <<< 3) + int64 b3)
        let candidates = candidates |> List.collect getNextCandidates |> List.filter isCorrectOutput
        if k = 0 then
            candidates[0]
        else
            loop candidates (k - 1)
    loop [0L] 15
    
let test =
    let sw = Stopwatch.StartNew()
    let day = 17

    let testCase r v = { IP = 0; Output = []; Registers = Map.ofList [(r, v)] }
    (interpretStep [2;6] (testCase RegisterId.C 9L)).Registers[RegisterId.B] |> should equal 1L
    (run (testCase RegisterId.A 10L) [5;0;5;1;5;4]).Output |> List.rev |> should equal [0;1;2]
    (run (testCase RegisterId.A 2024L) [0;1;5;4;3;0]).Output |> List.rev |> should equal [4;2;5;6;7;7;7;7;3;1;0]
    (run (testCase RegisterId.A 2024L) [0;1;5;4;3;0]).Registers[RegisterId.A] |> should equal 0L
    (interpretStep [1;7] (testCase RegisterId.B 29L)).Registers[RegisterId.B] |> should equal 26L
    (interpretStep [4;0] {IP=0; Output=[]; Registers=Map.ofList [(RegisterId.B, 2024L); (RegisterId.C, 43690L)]}).Registers[RegisterId.B] |> should equal 44354L

    let test0 = """
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
"""  
    calc1 (parse test0) |> should equal "4,6,3,5,6,3,5,2,1,0"
    
    let input = readDailyInput day
    calc1 (parse input) |> should equal "1,7,6,5,1,0,5,0,7"

    let state, program = parse input
    let quineA = calc2 program
    calc1 ({state with Registers=state.Registers.Add(RegisterId.A, quineA)}, program) |> should equal (String.Join(',', program))

    printfn $"Day {day} done: {sw.ElapsedMilliseconds}ms"
    
test
printfn $"Done"
