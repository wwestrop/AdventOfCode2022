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


//input <- File.ReadAllText("input.txt");

type range = (int * int)

let crateMatcher = Regex("(?<stack1>(   |\[[A-Z]\])) (?<stack2>(   |\[[A-Z]\])) (?<stack3>(   |\[[A-Z]\]))")
let instrMatcher = Regex("move (?<n>[0-9]+) from (?<from>[0-9]+) to (?<to>[0-9]+)")

let getStackRow(row: string) =
    let matches = crateMatcher.Matches(row)[0]
    matches.Groups["stack1"].Value[1], matches.Groups["stack2"].Value[1], matches.Groups["stack3"].Value[1]

let parseInstruction (instr: string) =
    let matches = instrMatcher.Matches(instr)[0]
    int (matches.Groups["n"].Value), int (matches.Groups["from"].Value), (int matches.Groups["to"].Value)

let day5 (input: string) =
    let input = input.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
    let stackGraphic = input.TakeWhile(fun x -> crateMatcher.IsMatch(x))
    let stackNumbers = input.SkipWhile(fun x -> crateMatcher.IsMatch(x)).First()        // part two will probably ask me to extend it to an arbitrary number of stacks
    let movesList = input.SkipWhile(fun x -> crateMatcher.IsMatch(x)).Skip(1)
    
    let stacks = [ Stack<char>(); Stack<char>(); Stack<char>() ]    // calling the ctor this way dones't seem idiomatic F# to me....... ?

    let stackRows = stackGraphic |> Seq.map getStackRow |> Seq.rev
    for (s1, s2, s3) in stackRows do
        if s1 <> ' ' then stacks[0].Push(s1)
        if s2 <> ' ' then stacks[1].Push(s2)
        if s3 <> ' ' then stacks[2].Push(s3)

    let instrs = movesList |> Seq.map parseInstruction
    for n, from, ``to`` in instrs do
        let fromStack = stacks[from - 1]
        let toStack = stacks[``to`` - 1]
        for _ in [0..n-1] do
            toStack.Push(fromStack.Pop())
        //let mutable pops = []
        //for _ in [0..n-1] do             // it's gonna ask me to pop 0 isn't it?
        //    //printfn "%i" j
        //    pops <- fromStack.Pop() :: pops         // mutable, plus seems less efficient than using a properly mutable list rather than recreating the variable
        //for p in pops do
        //    toStack.Push(p)

    let topsOfStacks = stacks[0].Peek(), stacks[1].Peek(), stacks[2].Peek()
    
    let (a, b, c) = topsOfStacks
    $"{a}{b}{c}"

printfn "%s" (day5 input)