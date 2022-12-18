module mod_day10
open System.IO
open System
open Helpers

let mutable input = "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"


input <- File.ReadAllText("input.txt");


let day10 (input: string) =

    let mutable valuesAtEndOfCycle: int list = [1]

    let handleNoop () =
        valuesAtEndOfCycle <- valuesAtEndOfCycle.Head :: valuesAtEndOfCycle

    let handleAddx operand =
        let r = valuesAtEndOfCycle.Head + operand
        valuesAtEndOfCycle <- valuesAtEndOfCycle.Head :: valuesAtEndOfCycle
        valuesAtEndOfCycle <- r :: valuesAtEndOfCycle

    let handleInstruction x =
        let instruction::args = x
        match instruction with
        | "noop" -> handleNoop ()
        | "addx" -> handleAddx (int args[0])


    for line in input.Split("\r\n", StringSplitOptions.RemoveEmptyEntries) do
        handleInstruction (Seq.toList <| line.Split(" "))

    valuesAtEndOfCycle <- valuesAtEndOfCycle |> List.rev

    let getSignalStrength cycle =
        valuesAtEndOfCycle[cycle - 1] * cycle

    let mutable s = 0
    for i in 20..40..valuesAtEndOfCycle.Length do
        s <- s + getSignalStrength i

    s



let day10part2 (input: string) =

    let mutable valuesAtEndOfCycle: int list = [1]

    let handleNoop () =
        valuesAtEndOfCycle <- valuesAtEndOfCycle.Head :: valuesAtEndOfCycle

    let handleAddx operand =
        let r = valuesAtEndOfCycle.Head + operand
        valuesAtEndOfCycle <- valuesAtEndOfCycle.Head :: valuesAtEndOfCycle
        valuesAtEndOfCycle <- r :: valuesAtEndOfCycle

    let handleInstruction x =
        let instruction::args = x
        match instruction with
        | "noop" -> handleNoop ()
        | "addx" -> handleAddx (int args[0])


    for line in input.Split("\r\n", StringSplitOptions.RemoveEmptyEntries) do
        handleInstruction (Seq.toList <| line.Split(" "))

    valuesAtEndOfCycle <- valuesAtEndOfCycle |> List.rev

    let hilite (x: int) (y: int) (i: bool) = i
    let tf (i: bool) = if i then "#" else "."
    let screen: bool[,] = Array2D.create 40 6 false


    let mutable cycle = 1
    for y in [1..6] do
        for x in [1..40] do
            let p = valuesAtEndOfCycle[cycle]
            if p = x-1 || p = x || p = x+1 then screen[x-1, y-1] <- true
            cycle <- cycle + 1

    Grid.PrintArray (screen, hilite, tf)



printfn "%i" (day10 input)
day10part2 input