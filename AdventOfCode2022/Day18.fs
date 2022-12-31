module mod_day18
open System.IO
open System

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


let getAdjacentPositions (point: int * int * int) (arr: 'T[,,]) =

    let offsetVectors = [
        -1, 0, 0;
         1, 0, 0;
         0,-1, 0;
         0, 1, 0;
         0, 0,-1;
         0, 0, 1;
    ]

    offsetVectors
        |> Seq.map (fun offset -> addVec point offset)


let day18 (input: string) =
    let grid, voxels = parse input

    voxels
        |> Seq.collect (fun v -> getAdjacentPositions v grid)
        |> Seq.filter(fun (x,y,z) -> not(isInBounds (x,y,z) grid) || grid[x,y,z] = false)
        |> Seq.length


printfn "%i" (day18 input)