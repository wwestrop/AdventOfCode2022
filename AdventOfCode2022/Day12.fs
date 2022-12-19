module mod_day12
open System.IO
open Helpers
open System.Drawing
open System

let mutable input = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"


input <- File.ReadAllText("input.txt");


let day12 (input: string) =

    let heightmap = Grid<int>.ParseGrid(input, (fun x -> int x))
    let w, h = heightmap.GetLength(0), heightmap.GetLength(1)

    let costToMove (from: Point) (xto: Point) =
        let oldHeight = heightmap[from.X, from.Y]
        let newHeight = heightmap[xto.X, xto.Y]

        if (newHeight <= oldHeight + 1) then 1 else 1000 // Int32.MaxValue 🤔 effectively infinite but not

    // F# really wants you to do recursion here, and I don't think it's the most straightforward way
    let findElement (map: int[,]) sought replacement =
        let mutable (rX, rY) = 0, 0
        for y in [0..h-1] do
            for x in [0..w-1] do
                if map[x, y] = int sought then map[x, y] <- int replacement; rX <- x; rY <- y
        Point(rX, rY)

    let findStart (map: int[,]) = findElement map 'S' 'a'
    let findEnd (map: int[,]) = findElement map 'E' 'z'

    let startPoint = findStart heightmap
    let endPoint = findEnd heightmap

    Djikstra.CalculateShortestPath (heightmap, startPoint, endPoint, costToMove)



let day12part2 (input: string) =

    let heightmap = Grid<int>.ParseGrid(input, (fun x -> int x))
    let w, h = heightmap.GetLength(0), heightmap.GetLength(1)

    let costToMove (from: Point) (xto: Point) =
        let oldHeight = heightmap[from.X, from.Y]
        let newHeight = heightmap[xto.X, xto.Y]

        if (newHeight <= oldHeight + 1) then 1 else 1000 // Int32.MaxValue 🤔 effectively infinite but not

    // F# really wants you to do recursion here, and I don't think it's the most straightforward way
    let findElement (map: int[,]) sought replacement =
        let mutable (rX, rY) = 0, 0
        for y in [0..h-1] do
            for x in [0..w-1] do
                if map[x, y] = int sought then map[x, y] <- int replacement; rX <- x; rY <- y
        Point(rX, rY)

    let findStart (map: int[,]) = findElement map 'S' 'a'
    let findEnd (map: int[,]) = findElement map 'E' 'z'

    let startPoint = findStart heightmap
    let endPoint = findEnd heightmap


    let mutable shortestSeen = Int32.MaxValue
    for y in [0..h-1] do
        for x in [0..w-1] do
            if heightmap[x, y] = int 'a' then do
                shortestSeen <- min shortestSeen (Djikstra.CalculateShortestPath (heightmap, Point(x, y), endPoint, costToMove))
    
    shortestSeen



printfn "%i" (day12 input)
printfn "%i" (day12part2 input)