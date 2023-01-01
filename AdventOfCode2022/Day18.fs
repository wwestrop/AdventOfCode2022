module mod_day18
open System.IO
open System
open Helpers

let mutable input = "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"



input <- File.ReadAllText("input.txt");



let parseLine (line: string) = 
    let parts = line.Split(',') |> Seq.map int |> Seq.toArray
    parts[0], parts[1], parts[2]



let parse (input: string) =
    let lines =
        input.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map parseLine

    let w,_,_ = lines |> Seq.maxBy (fun (x,_,_) -> x)
    let _,h,_ = lines |> Seq.maxBy (fun (_,y,_) -> y)
    let _,_,d = lines |> Seq.maxBy (fun (_,_,z) -> z)

    let voxels = Array3D.create (w + 1) (h + 1) (d + 1) false

    for x,y,z in lines do
        voxels[x,y,z] <- true

    voxels, lines


let addVec (x1,y1,z1) (x2,y2,z2) =
    x1+x2, y1+y2, z1+z2


let isInBounds (x: int, y: int, z: int) (arr: 'T[,,]) =
    let w, h, d = arr.GetLength(0), arr.GetLength(1), arr.GetLength(2)

    if x < 0 || y < 0 || z < 0 then
        false
    else
        x < w && y < h && z < d


let right = (1,0,0)
let left = (-1,0,0)
let up = (0,1,0)
let down = (0,-1,0)
let forward = (0,0,1)
let backward = (0,0,-1)

let allDirections = [right; left; up; down; forward; backward]


let getAdjacentPositions (point: int * int * int) (arr: 'T[,,]) =
    allDirections
        |> Seq.map (fun offset -> addVec point offset)



let day18 (input: string) =
    let grid, voxels = parse input

    voxels
        |> Seq.collect (fun v -> getAdjacentPositions v grid)
        |> Seq.filter(fun (x,y,z) -> not(isInBounds (x,y,z) grid) || grid[x,y,z] = false)
        |> Seq.length



// Even checking for surrounding blocks in each direction won't work in all cases
// Consider a 2D equivalent ( . = air, # = solid ):
// 
//   01234567
// 0 ........
// 1 ..#####.
// 2 ..##.##.
// 3 ..##.##.
// 4 ..#..##.
// 5 ..#.###.
// 6 ..# ###.
// 7 ........
//
// Consider (4,2). It has voxels both above and below it, and to the left and right
// And if it was 3D, lets assume in front and behind it too.
// But, the "kink" in the tunnel at (3,4) means that (4,2) is, in fact, exposed to air
//
// Do I need to Dijkstra or something to find the distance to any of the edges of the scanned volume?
//
let populateSurroundedVoxels (voxels: seq<int * int * int>) (arr: bool[,,]) =
    let mutable copyVoxels = voxels |> Seq.toList
    let w,h,d = arr.GetLength(0), arr.GetLength(1), arr.GetLength(2)

    for x in 0..w-1 do
        for y in 0..h-1 do
            for z in 0..d-1 do
                let isVoxelSurrounded =
                    allDirections
                    |> Seq.map (fun dir -> getCellsInDirection3D (x,y,z) dir arr)
                    |> Seq.forall (fun x -> Seq.contains true x)

                if isVoxelSurrounded then
                    if arr[x,y,z] = false then
                        arr[x,y,z] <- true
                        copyVoxels <- (x,y,z) :: copyVoxels

    copyVoxels


let day18part2 (input: string) =

    let grid, voxels = parse input

    let voxels = populateSurroundedVoxels voxels grid

    voxels
    |> Seq.collect (fun v -> getAdjacentPositions v grid)
    |> Seq.filter(fun (x,y,z) -> not(isInBounds (x,y,z) grid) || grid[x,y,z] = false)
    |> Seq.length



printfn "%i" (day18 input)
printfn "%i" (day18part2 input)
