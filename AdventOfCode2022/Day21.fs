module mod_day21
open System.IO
open System
open System.Collections.Generic
open System.Text.RegularExpressions

let mutable input = "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"


input <- File.ReadAllText("input.txt");


//type Monkey = { 
//    name: string;
//    canSolve: unit -> bool;         // TODO combine solve/canSolve into an option type
//    solve: unit -> uint64;
//    descendents: (string * string) option;
//}

type Monkey(name: string, canSolve: unit -> bool, solve: unit -> uint64, descendents: (string * string) option) =
    member this.name = name
    member this.canSolve = canSolve
    member this.solve = solve
    member this.descendents = descendents


let mutable solved: IDictionary<string, uint64> = Dictionary<string, uint64>()


let parseMathOp x =
    let regex = Regex "^(?<num1>[a-z]{4}) (?<op>[+-/*]) (?<num2>[a-z]{4})$"

    if not (regex.IsMatch x) 
        then None
        else
            let matches = regex.Matches(x)[0]
            let tup = matches.Groups["num1"].Value, matches.Groups["op"].Value, matches.Groups["num2"].Value
            Some tup


let isConst x =
    let regex = Regex "^[0-9]+$"
    if regex.IsMatch x then Some (uint64 x) else None

let solve (op1, op, op2) =
    let op1 = solved[op1]
    let op2 = solved[op2]

    let opFunc = match op with
    | "+" -> (+)
    | "-" -> (-)
    | "/" -> (/)
    | "*" -> (*)

    opFunc op1 op2


let parse (line: string) =
    let monkeyName::operation::_ = line.Split(": ") |> Seq.toList

    let constOp = isConst operation
    let mathOp = parseMathOp operation
    
    let canSolve =
        match constOp, mathOp with
        | Some c, None -> fun () -> true
        | None, Some (op1, _, op2) -> fun () -> solved.ContainsKey(op1) && solved.ContainsKey(op2)

    let solve =
        match constOp, mathOp with
        | Some c, None -> fun () -> c
        | None, Some m -> fun () -> solve m

    let descendents =
        match mathOp with
        | None -> None
        | Some (o1, _, o2) -> Some (o1, o2)

    new Monkey(monkeyName, canSolve, solve, descendents)


// TODO built in alternative to `dict` which is mutable? Or is this just not encouraged?
let mutdict (d: ('K * 'V) seq): IDictionary<'K, 'V> =
    let result = new Dictionary<'K, 'V>()
    for k, v in d do
        result[k] <- v

    result


let day21 (input: string) =

    let monkeys = 
        input.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map parse

    let allMonkeys = dict (monkeys |> Seq.map (fun x -> x.name, x))

    // TODO meaningful whitespace is extremely annoying to me.
    // TODO what's the idiomatic way of formatting this to make it clear?
    solved <- mutdict (
        allMonkeys.Values
        |> Seq.filter (fun x -> x.canSolve())
        |> Seq.map (fun x -> x.name, x.solve())
        )

    while not(allMonkeys["root"].canSolve()) do
        let unsolved = 
            allMonkeys.Values
            |> Seq.filter (fun x -> not(solved.ContainsKey(x.name)))

        for x in unsolved do
            if x.canSolve() then
                solved[x.name] <- x.solve()

    allMonkeys["root"].solve()



printfn "%i" (day21 input)