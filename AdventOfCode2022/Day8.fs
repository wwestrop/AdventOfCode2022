module mod_day8
open System.IO
open Helpers

let mutable input = "30373
25512
65332
33549
35390"


input <- File.ReadAllText("input.txt");



let day8 (input: string) =

    let hilite (x: int) (y: int) (i: byte) = i = 0uy
    let tf (i: byte) = if i = 0uy then "X" else "-"

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



printfn "%i" (day8 input)