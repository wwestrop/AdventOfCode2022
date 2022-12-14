module mod_day5
open System.IO
open System.Linq
open System.Text.RegularExpressions
open System
open System.Collections.Generic

let mutable input = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"


input <- File.ReadAllText("input.txt");

type range = (int * int)

let crateMatcher = Regex("((?<stack>(   |\[[A-Z]\])) ?)+")
let instrMatcher = Regex("move (?<n>[0-9]+) from (?<from>[0-9]+) to (?<to>[0-9]+)")

let getStackRow(row: string) =
    let matches = crateMatcher.Matches(row)[0]
    matches.Groups["stack"].Captures |> Seq.map (fun x -> x.Value[1]) |> Seq.toArray

let parseInstruction (instr: string) =
    let matches = instrMatcher.Matches(instr)[0]
    int (matches.Groups["n"].Value), int (matches.Groups["from"].Value), (int matches.Groups["to"].Value)

let initialiseStacks (initialStackGraphic: seq<char[]>) =
    let numStacks = initialStackGraphic.First().Count()
    let stacks = [1..numStacks] |> Seq.map (fun _ -> Stack<char>()) |> Seq.toArray       // calling the ctor this way dones't seem idiomatic F# to me....... ?  

    for row in initialStackGraphic do
        let numberedColumns = [0..row.Count()-1] |> Seq.map (fun x -> x, row[x]) |> Seq.toArray
        for (n, box) in numberedColumns do
            if box <> ' ' then stacks[n].Push(box)

    stacks

let topsOfStacks (stacks: Stack<char>[]) =
    let stackTops = stacks |> Seq.map (fun x -> x.Peek())
    String.Join("", stackTops)

let isColumnNumbers (row: string) = 
    let (b, _) = Int32.TryParse(row.Replace(" ", ""))
    b

let day5 (input: string) =
    let input = input.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
    let stackGraphic = input.TakeWhile(fun x -> not (isColumnNumbers x))
    let movesList = input.SkipWhile(fun x -> crateMatcher.IsMatch(x) || isColumnNumbers x)
    
    let stackRows = stackGraphic |> Seq.map getStackRow |> Seq.rev
    let stacks = initialiseStacks stackRows

    let instrs = movesList |> Seq.map parseInstruction
    for n, from, ``to`` in instrs do
        let fromStack = stacks[from - 1]
        let toStack = stacks[``to`` - 1]
        for _ in [1..n] do
            toStack.Push(fromStack.Pop())

    topsOfStacks stacks

printfn "%s" (day5 input)