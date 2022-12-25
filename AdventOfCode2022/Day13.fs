module mod_day13
open System.IO
open System

let mutable input = "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"


input <- File.ReadAllText("input.txt");


type Atom = AInt of int | AList of Atom list


// TODO converting constantly between the wrapped types in the union and their unwrapped types
// is annoying. Is there a better way to do this? esp when pattern matching on type is happening
// And figure out why can't I use the union cases in all places, sometimes have to use the alias, sometimes the inner type


let rec order i1 i2 =
    // positive -> a1 bigger
    match i1, i2 with
    | AInt a1, AInt a2 -> a1 - a2
    | AList _, AList [] -> 1             
    | AList [], AList _ -> -1             
    | AList a1, AList a2 -> if a1.Head = a2.Head
                            then order (AList a1.Tail) (AList a2.Tail)
                            else order a1.Head a2.Head
    | AList a1, AInt a2 -> order (AList a1) (AList [AInt a2])
    | AInt a1, AList a2 -> order (AList [AInt a1]) (AList a2)


let findClosingBracket (str: string) (startingBracket: int) =

    let mutable bracketCount = 0
    let mutable shouldBreak = 0             // TODO hack, not idiomatic

    let mutable i = startingBracket
    while shouldBreak = 0 && i < str.Length do
        if str[i] = '[' then
            bracketCount <- bracketCount + 1
        else if str[i] = ']' then
            bracketCount <- bracketCount - 1
            if bracketCount = 0 then shouldBreak <- i
        i <- i + 1

    shouldBreak

let rec parseList (l: string) =
    let mutable acc = ""
    let mutable r: Atom list = []

    let mutable i = 0
    while i < l.Length do
        let c = l[i]
        if c = '[' then do
            let ls = findClosingBracket l i
            let l2 = l[(i+1)..(ls-1)]
            let innerList = parseList l2
            i <- ls
            r <- AList innerList :: r
        else if c = ',' then do
            if acc <> "" then r <- AInt(int acc) :: r
            acc <- ""
        else do
            acc <- acc + c.ToString()

        i <- i + 1      // since we're already mutating the loop variable, this gets double done in one case
                        // should probably use a parser generator, but am abusing the language to the max for learing purposes

    if (acc <> "") then do
        r <- AInt(int acc) :: r
        acc <- ""

    r |> List.rev           // TODO build the list the other way around in F#

let pair (source: 'T seq) =

    //let t1: 'T option = None          // TODO what is the difference between uppercase and lowercase
    //let t2: 'T Option = None

    let mutable p1 = None
    let mutable p2 = None
    let mutable a = true

    seq {
        for x in source do
            if a then p1 <- Some x else p2 <- Some x
            if not a then yield p1.Value, p2.Value
            a <- not a
    }


let day13 (input: string) =

    let leftIsSmaller (l, r) =
        let i = order l r
        i < 0

    input.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map parseList
        |> Seq.map (fun x -> x[0])
        |> pair
        |> Seq.mapi (fun i x -> i+1, leftIsSmaller x)
        |> Seq.filter (fun (i,x) -> x)
        |> Seq.sumBy (fun (i,_) -> i)


printfn "%i" (day13 input)