module AdventArray

open Utility

let parseArray2D inputText padding padChar =
    let lines = parseLines inputText
    let ydim = lines.Length
    let xdim = lines |> List.map String.length |> List.distinct |> List.exactlyOne
    let arr = Array2D.init xdim ydim (fun x y -> lines[y][x])
    if padding = 0 then
        arr
    else
        let arr2 = Array2D.create (xdim + 2 * padding) (ydim + 2 * padding) padChar
        Array2D.blit arr 0 0 arr2 padding padding xdim ydim
        arr2

let parseIntArray2D inputText padding =
    let lines = parseLines inputText
    let ydim = lines.Length
    let xdim = lines |> List.map String.length |> List.distinct |> List.exactlyOne
    let arr = Array2D.init xdim ydim (fun x y -> int (string (lines[y][x])))
    if padding = 0 then
        arr
    else
        let arr2 = Array2D.create (xdim + 2 * padding) (ydim + 2 * padding) -1
        Array2D.blit arr 0 0 arr2 padding padding xdim ydim
        arr2

let directions8 = [ for dx in -1..1 do for dy in -1..1 do (dx, dy) ] |> List.except [(0, 0)]
let directions4 = [ (0,-1) ; (1,0) ; (0,1) ; (-1,0) ]

let maxes arr padding = (Array2D.length1 arr - padding - 1), (Array2D.length2 arr - padding - 1)

let addvec (x, y) (dx, dy) = (x + dx, y + dy)

let vec p1 p2 =
    let x1, y1 = p1
    let x2, y2 = p2
    (x2 - x1, y2 - y1)

let item pos (arr:char array2d) = arr[fst pos, snd pos]

let set char pos (arr:char array2d) = arr[fst pos, snd pos] <- char

let find char arr =
    let mutable pos = (-1, -1)
    arr |> Array2D.iteri (fun i j c -> if c = char then pos <- (i, j))
    if pos = (-1, -1) then failwithf "Not found" else pos

let count char (arr:char array2d)= arr |> Seq.cast<char> |> Seq.filter (fun cc -> cc = 'X') |> Seq.length 
