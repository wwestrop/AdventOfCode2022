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
//    solve: unit -> int64;
//    descendents: (string * string) option;
//}

type Operator = Plus | Minus | Multiply | Divide

type Monkey(name: string, canSolve: unit -> bool, solve: unit -> int64, operator: Operator option, descendents: (string * string) option) =
    member this.name = name
    member this.canSolve = canSolve
    member this.solve = solve
    member this.operator = operator
    member this.descendents = descendents


let mutable solved: IDictionary<string, int64> = Dictionary<string, int64>()


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
    if regex.IsMatch x then Some (int64 x) else None

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

    let operator = 
        match mathOp with
        | None -> None
        | Some (_, "+", _) -> Some Plus
        | Some (_, "-", _) -> Some Minus
        | Some (_, "*", _) -> Some Multiply
        | Some (_, "/", _) -> Some Divide


    new Monkey(monkeyName, canSolve, solve, operator, descendents)


// TODO built in alternative to `dict` which is mutable? Or is this just not encouraged?
let mutdict (d: ('K * 'V) seq): IDictionary<'K, 'V> =
    let result = new Dictionary<'K, 'V>()
    for k, v in d do
        result[k] <- v

    result


let solveAllTerms (allMonkeys: IDictionary<string, Monkey>) =

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

    ()


let day21 (input: string) =

    let allMonkeys = 
        input.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map parse
        |> Seq.map (fun x -> x.name, x)
        |> dict

    solveAllTerms allMonkeys

    allMonkeys["root"].solve()


let rec findOperand (soughtOperand: string) (searchFrom: Monkey) (allMonkeys: IDictionary<string,Monkey>) =
    if searchFrom.name = soughtOperand then
        true
    else
        match searchFrom.descendents with
        | None -> false
        | Some (op1, op2) -> findOperand soughtOperand allMonkeys[op1] allMonkeys 
                             || findOperand soughtOperand allMonkeys[op2] allMonkeys


let rec goalSeek (searchFrom: Monkey) (soughtNode: string) (target: int64) (allMonkeys: IDictionary<string,Monkey>) =

    let lhs, rhs = searchFrom.descendents.Value

    let isSoughNodeLhs = findOperand soughtNode allMonkeys[lhs] allMonkeys
    let knownOperand =
        match isSoughNodeLhs with
        | true -> allMonkeys[rhs].solve()
        | false -> allMonkeys[lhs].solve()

    let calculatedOtherOperandValue = 
        match searchFrom.operator with
        | Some Plus -> target - knownOperand
        | Some Minus -> if isSoughNodeLhs then target + knownOperand else knownOperand - target     // TODO need to be careful about operator ordering here
        | Some Multiply -> target / knownOperand
        | Some Divide -> if isSoughNodeLhs then target * knownOperand else knownOperand / target
        //| None -> 1L        // TODO what do I do here? am I avoiding leaf nodes altogether?

    // only recurse if humn is not either of our descendents (if it is just return that value, we've found it)
    if lhs = soughtNode || rhs = soughtNode then
        calculatedOtherOperandValue
    else
        // recurse down
        goalSeek allMonkeys[if isSoughNodeLhs then lhs else rhs] soughtNode calculatedOtherOperandValue allMonkeys

let day21part2 (input: string) =

    let allMonkeys = 
        input.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map parse
        |> Seq.map (fun x -> x.name, x)
        |> mutdict

    solveAllTerms allMonkeys

    let root = allMonkeys["root"]
    let lhs, rhs = root.descendents.Value
    let isLhs = findOperand "humn" allMonkeys[lhs] allMonkeys

    let theValueWeAreTryingToMatch = allMonkeys[if isLhs then rhs else lhs].solve()
        //match isLhs with
        //| true -> allMonkeys[rhs].solve()
        //| false -> allMonkeys[lhs].solve()

    goalSeek allMonkeys[if isLhs then lhs else rhs] "humn" theValueWeAreTryingToMatch allMonkeys



printfn "%i" (day21 input)
printfn "%i" (day21part2 input)