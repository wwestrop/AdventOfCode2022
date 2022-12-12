module mod_day3
open System.IO
open System

let mutable input = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"


input <- File.ReadAllText("input.txt");



let day3 (input: string) =

    let lines = input.Replace(" ", "").Split("\r\n")

    let parse (line: string) =
        let compartmentSize = line.Length / 2
        let compartment1 = line[..compartmentSize - 1].ToCharArray()
        let compartment2 = line[compartmentSize..]
        compartment1, compartment2

    let rucksacks =
        seq {
            for l in lines -> parse l
        }

    let getPriority (i: char) =
        uint (if i >= 'a' && i <= 'z' then byte i - 96uy else byte i - 64uy + 26uy)

    let getDupesAcrossCompartments (rucksack: char[] * string) =
        let (c, o) = rucksack
        c |> Array.find (fun x -> o.Contains(x, StringComparison.InvariantCulture))

    rucksacks
        |> Seq.map getDupesAcrossCompartments 
        |> Seq.map getPriority
        |> Seq.sum



printfn "%i" (day3 input)