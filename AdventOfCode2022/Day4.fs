module mod_day4
open System.IO
open System.Linq

let mutable input = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"


input <- File.ReadAllText("input.txt");

type range = (int * int)


let day4 (input: string) =

    let getRange (r: string) =
        let rangeStartEnd = r.Split("-").Select(fun x -> int x)
        range (rangeStartEnd.ElementAt(0), rangeStartEnd.ElementAt(1))

    let isCompletelyCoveredBy (coveree: range) (coverer: range) =
        let (minA, maxA) = coveree          // ugh, this destructing again. did the type declaration help? what is the proper way of doing this?
        let (minB, maxB) = coverer
        minB <= minA && maxB >= maxA

    // This feels very C#, am I cheating? 😂
    //let sectionPairs = input.Replace(" ", "").Split("\r\n").Select(fun x -> x.Split(","))

    let lines =
        input.Replace(" ", "").Split("\r\n")
        |> Seq.map (fun x -> x.Split(","))
        |> Seq.map (fun x -> x[0], x[1])
        |> Seq.map (fun (x, y) -> getRange x, getRange y)
        |> Seq.filter (fun (x, y) -> isCompletelyCoveredBy x y || isCompletelyCoveredBy y x)

    lines.Count()



printfn "%i" (day4 input)