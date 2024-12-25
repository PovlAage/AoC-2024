module AdventGraph

open System
open System.Collections.Generic
open Utility

type Neighbour<'T> = Neighbour of 'T * int64
type Node<'T> = { State: 'T; Neighbours: Neighbour<'T> list}
type Graph<'T when 'T : comparison> = Map<'T, Neighbour<'T> list> 

let dijkstra<'T when 'T : comparison> (graph:Graph<'T>) (startPos:'T) =
    // init
    let vertices = graph |> Map.toList |> List.map fst
    let mutable dist = vertices |> Seq.map (fun v -> v, Int64.MaxValue) |> Map.ofSeq
    dist <- dist.Add(startPos, 0L)
    let mutable prev = Map.empty            
    let queue = PriorityQueue<'T, int64>()
    queue.Enqueue(startPos, 0L)
    
    while queue.Count > 0 do
        let u = queue.Dequeue()
        let neighbours = graph |> Map.find u
        for Neighbour (v, d) in neighbours do
            let alt = dist[u] + d
            if alt < dist[v] then
                prev <- prev |> Map.add v [u]
                dist <- dist |> Map.add v alt
                queue.Enqueue(v, alt)

    dist

let astar<'T when 'T : comparison> (graph:Graph<'T>) (startPos:'T) (isGoal:'T->bool) h=
    let openSet = PriorityQueue<'T, int64>()
    openSet.Enqueue(startPos, h(startPos))

    let mutable cameFrom = Map.empty
    let mutable gScore = Map [startPos, 0L]
    
    let rec loop current =
        let gScoreCurrent = gScore[current]
        if isGoal current then
            gScoreCurrent
        else
            let neighbours = graph |> Map.find current
            for Neighbour (v, d) in neighbours do
                let tentative_gScore = gScoreCurrent + d
                match gScore |> Map.tryFind v with
                | Some g when g <= tentative_gScore -> ()
                | _ ->
                    cameFrom <- cameFrom |> Map.add v current
                    gScore <- gScore |> Map.add v tentative_gScore
                    let fScoreNeighbour = tentative_gScore + h(v)
                    openSet.Enqueue(v, fScoreNeighbour)
            if openSet.Count > 0 then
                loop (openSet.Dequeue())
            else
                -1

    loop startPos
