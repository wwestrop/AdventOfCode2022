module mod_day6
open System.Linq
open System.IO

let mutable input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"


input <- File.ReadAllText("input.txt");


let allDifferentLetters (input: string) =
    let chars = input.ToCharArray() |> Seq.distinct |> Seq.toList
    chars.Count() = 4
    
let substr4 (input: string) (offset: int) = input.Substring(offset, 4)

let day6 (input: string) =
    let markerSequence =
        [0..input.Length-4]
        |> Seq.map (substr4 input)
        |> Seq.filter allDifferentLetters
        |> Seq.head

    input.IndexOf markerSequence + 4        // be more efficient to capture the offset as we go rather than look it up again



printfn "%i" (day6 input)
//printfn "%i" (day6 "bvwbjplbgvbhsrlpgdmjqwftvncz")          // 5
//printfn "%i" (day6 "nppdvjthqldpwncqszvftbrmjlhg")          // 6
//printfn "%i" (day6 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")     // 10
//printfn "%i" (day6 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")      // 11