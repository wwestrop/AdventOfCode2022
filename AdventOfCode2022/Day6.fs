module mod_day6
open System.Linq
open System.IO

let mutable input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"


input <- File.ReadAllText("input.txt");


let allDifferentLetters (input: string) =
    let chars = input.ToCharArray() |> Seq.distinct |> Seq.toList
    chars.Count() = input.Length
    
let substr (input: string) (seqLength: int) (offset: int) = input.Substring(offset, seqLength)

let findSeq (input: string) (seqLength: int) =
    let markerSequence =
        [0..input.Length-seqLength]
        |> Seq.map (substr input seqLength)
        |> Seq.filter allDifferentLetters
        |> Seq.head

    input.IndexOf markerSequence + seqLength        // be more efficient to capture the offset as we go rather than look it up again


let day6 (input: string) = findSeq input 4
let day6part2 (input: string) = findSeq input 14


printfn "%i" (day6 input)
//printfn "%i" (day6 "bvwbjplbgvbhsrlpgdmjqwftvncz")          // 5
//printfn "%i" (day6 "nppdvjthqldpwncqszvftbrmjlhg")          // 6
//printfn "%i" (day6 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")     // 10
//printfn "%i" (day6 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")      // 11

printfn "%i" (day6part2 input)
//printfn "%i" (day6part2 "bvwbjplbgvbhsrlpgdmjqwftvncz")          // 23
//printfn "%i" (day6part2 "nppdvjthqldpwncqszvftbrmjlhg")          // 23
//printfn "%i" (day6part2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")     // 29
//printfn "%i" (day6part2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")      // 26