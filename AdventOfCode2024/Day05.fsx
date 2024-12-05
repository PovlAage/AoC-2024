#r "nuget: xunit, 2.6.6"
#r "nuget: FsUnit.xUnit, 6.0.1"
#load "Utility.fs"

open Utility
open FsUnit.Xunit

type Rule = int * int
type Update = int array

let parseRules (ruleBlock:string list) =
    ruleBlock |> List.map (split2 '|') |> List.map (fun (p1, p2) -> (int p1, int p2)) 

let parseUpdates (updateBlock:string list) =
    updateBlock |> List.map (fun s -> s.Split(',') |> Array.map int)

let findNonMatching (update:Update) rule =
    let tryFindIndex i = update |> Array.tryFindIndex (fun e -> e = i)
    match tryFindIndex (fst rule), tryFindIndex (snd rule) with
    | Some i1, Some i2 when i2 < i1 -> Some (i1, i2)
    | _ -> None

let conformsTo update rule =
    (findNonMatching update rule).IsNone

let conformsToAll rules update =
    rules |> List.forall (conformsTo update)

let mid (a:int array) = a[a.Length / 2]

let calc1 input =
    let b1, b2 = parseBlocks2 input
    let rules = parseRules b1
    let updates = parseUpdates b2
    let correct, _ = updates |> List.partition (conformsToAll rules)
    correct |> List.sumBy mid

let calc2 input =
    let b1, b2 = parseBlocks2 input
    let rules = parseRules b1
    let updates = parseUpdates b2
    let _, incorrect = updates |> List.partition (conformsToAll rules)
    let swap i1 i2 (u:int array) =
        let e1 = u[i1]
        u[i1] <- u[i2]
        u[i2] <- e1
        
    let bubble (update:Update) =
        let rec loop u =
            match rules |> List.choose (findNonMatching u) with
            | [] -> u
            | (i1, i2) :: tail -> swap i1 i2 u ; loop u
        loop update

    incorrect |> List.map bubble |> List.sumBy mid

let test =
    let day = 5
    let test1 = """
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"""
    calc1 test1 |> should equal 143

    let input = readDailyInput day
    calc1 input |> should equal 5091
    
    calc2 test1 |> should equal 123
    calc2 input |> should equal 4681
    
test    
