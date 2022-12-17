module mod_day8
open System.IO
open Helpers

let mutable input = "30373
25512
65332
33549
35390"


input <- File.ReadAllText("input.txt");


let hilite (x: int) (y: int) (i: byte) = i = 0uy
let tf (i: byte) = if i = 0uy then "X" else "-"



let day8 (input: string) =

    let arr: int[,] = Grid.ParseNumberGrid input
    let (w, h) = arr.GetLength(0), arr.GetLength(1)
    let visibilityMap = Array2D.create w h 4uy

    let setVisibiltyFromWest (row: int) =
        let mutable highestTree = arr[0, row]
        for c in 1..w-2 do
            let t = arr[c, row]
            if t <= highestTree then do
                visibilityMap[c, row] <- visibilityMap[c, row] - 1uy
            else highestTree <- t

    let setVisibiltyFromEast (row: int) =
        let mutable highestTree = arr[w-1, row]
        for c in w-2..-1..1 do
            let t = arr[c, row]
            if t <= highestTree then do
                visibilityMap[c, row] <- visibilityMap[c, row] - 1uy
            else highestTree <- t

    let setVisibiltyFromNorth (col: int) =
        let mutable highestTree = arr[col, 0]
        for r in 1..h-2 do
            let t = arr[col, r]
            if t <= highestTree then do
                visibilityMap[col, r] <- visibilityMap[col, r] - 1uy
            else highestTree <- t

    // TODO rather than access the array col/rows directly, give it just the thing it needs, as a "stream of values", then the same function can be used for each cardinal direction (though what you feed as input differs)
    let setVisibiltyFromSouth (col: int) =
        let mutable highestTree = arr[col, h-1]
        for r in h-2..-1..1 do
            let t = arr[col, r]
            if t <= highestTree then do
                visibilityMap[col, r] <- visibilityMap[col, r] - 1uy
            else highestTree <- t

    for row in 1..h-2 do
        setVisibiltyFromWest row
        setVisibiltyFromEast row

    for col in 1..w-2 do
        setVisibiltyFromNorth col
        setVisibiltyFromSouth col

    // TODO this is notable, and I still don't fully comprehend it. C# interop
    // Seemingly cannot omit optional arguments, as F# treats it as partial application
    // Also need to provide commas between args unlike for normal F# funcs, not sure for the reason. It it tupling it before sending it to C#?
    Grid.PrintArray (visibilityMap, hilite, tf)

    // TODO nicer way of counting everything in an array?
    let mutable visibleTrees = 0
    for row in 0..h-1 do
        for col in 0..w-1 do
            if visibilityMap[col, row] <> 0uy then visibleTrees <- visibleTrees + 1

    visibleTrees


let day8part2 (input: string) =

    let arr: int[,] = Grid.ParseNumberGrid input
    let (w, h) = arr.GetLength(0), arr.GetLength(1)
    let scenicScores = Array2D.create w h 0u

    let getTreeSequence (starting: int * int) xoffset yoffset =
        seq {
            let mutable x, y = starting
            x <- x + xoffset
            y <- y + yoffset

            while x >= 0 && x <= w-1 && y >= 0 && y <= h-1 do
                yield arr[x, y]
                x <- x + xoffset
                y <- y + yoffset
        }

    let hackToCaptureTheLastElement p =
        let mutable doOneMore = true

        fun value ->
            let o = p value
            match doOneMore with
            | false -> false
            | true when doOneMore && o -> true
            | _ -> doOneMore <- false; true

    let north = (0, -1)
    let south = (0, 1)
    let east = (1, 0)
    let west = (-1, 0)

    let getScoreInDirection (col, row) (xoffset, yoffset) =
        let t = arr[col, row]

        getTreeSequence (col, row) xoffset yoffset
        |> Seq.takeWhile (hackToCaptureTheLastElement (fun x -> x < t))
        |> Seq.length
        |> uint

    let getScore col row =
        getScoreInDirection (col, row) north
        * getScoreInDirection (col, row) south
        * getScoreInDirection (col, row) east
        * getScoreInDirection (col, row) west

    let mutable highestScore = 0u
    for row in 0..h-1 do
        for col in 0..w-1 do
            let s = getScore col row
            scenicScores[col, row] <- s
            if s > highestScore then highestScore <- s

    highestScore



printfn "%i" (day8 input)
printfn "%i" (day8part2 input)