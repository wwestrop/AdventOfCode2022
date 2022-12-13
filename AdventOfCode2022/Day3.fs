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



let getPriority (i: char) =
    uint (if i >= 'a' && i <= 'z' then byte i - 96uy else byte i - 64uy + 26uy)



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

    let findCommonLetter (rucksack: char[] * string) =
        let (c, o) = rucksack
        c |> Array.find (fun x -> o.Contains(x, StringComparison.InvariantCulture))

    rucksacks
        |> Seq.map findCommonLetter
        |> Seq.map getPriority
        |> Seq.sum



let day3part2 (input: string) =

    let rucksacks = input.Replace(" ", "").Split("\r\n")

    let elfGroups =
        seq {
            let mutable count = 0;              // this is a bit imperative, is there a more functional way?
            let mutable x = []

            for l in rucksacks do
                x <- l :: x
                count <- count + 1
                match count with
                | 3 -> yield x; count <- 0
                | _ -> ()
        }

    // Overloads with the same name not allowed in same scope, latest wins??? Maybe another call for currying?
    let findCommonLetter (elfGroup: string list) =      // Using the product type (?) above fits more easily within Seq.map. I guess we need to curry it if we need to be only passing one argument in the |> chain?????
        let e1 = elfGroup[0].ToCharArray()
        let e2 = elfGroup[1].ToCharArray()
        let e3 = elfGroup.[2].ToCharArray()     // [i] vs .[i]  -- is there any difference? Prior to F# 6, the syntax expr.[idx] was used for indexing
        // a 3-tuple would probably be neater

        let commonLetters = Set e1 |> Set.intersect (Set e2) |> Set.intersect (Set e3) |> Set.toList
        commonLetters |> List.head      // our rubric says only one duplicated item across rucksacks

        //System.Linq.Enumerable.Intersect(e1, e2)          <-- is there a nice way of doing this? Does F# only allow it on sets?

    elfGroups
        |> Seq.map findCommonLetter
        |> Seq.map getPriority
        |> Seq.sum



printfn "%i" (day3 input)
printfn "%i" (day3part2 input)