module mod_day2     // this is necessary for the namespace to work, for some reason
open System.IO

let mutable input = "A Y
B X
C Z"


input <- File.ReadAllText("input.txt");


let lookup = Map [ 
    ('A', "rock")
    ('B', "paper")
    ('C', "scissors")
    ('X', "rock")
    ('Y', "paper")
    ('Z', "scissors")
]

let leftHandWins = [
    ("rock", "scissors")
    ("paper", "rock")
    ("scissors", "paper")
]

let allMoves = [ "rock"; "paper"; "scissors" ]


let getWinningMove move =
    let (l, _) = leftHandWins |> List.filter (fun (_, r) -> r = move) |> List.head
    l



let getLosingMove (move: string) =
    let winningMove = getWinningMove move
    allMoves |> List.except [move] |> List.except [winningMove] |> List.head



let getCorrespondingMove theirMove desiredOutcome =
    match desiredOutcome with
    | 'Y' -> theirMove                      // draw
    | 'Z' -> getWinningMove theirMove       // win
    | 'X' -> getLosingMove theirMove        // lose
 


let score (round: string * string) =
    let shapeScore =
        match round with
        | _, "rock" -> 1
        | _, "paper" -> 2
        | _, "scissors" -> 3

    let outcomeScore =
        let (x, _) = round             // this is rindonkulously painful
        let (_, y) = round

        let isDraw = x = y            
        let isLoss = List.contains round leftHandWins

        match isDraw, isLoss with
        | true, _ -> 3
        | _, true -> 0
        | _ -> 6

    shapeScore + outcomeScore


let day2 (input: string) =

    let lines = input.Replace(" ", "").Split("\r\n")

    let parse (line: string) =
        lookup.[line[0]], lookup.[line[1]]

    let roundsPlayed =
        seq {
            for l in lines -> parse l
        }

    roundsPlayed |> Seq.map score |> Seq.sum



let day2part2 (input: string) =

    let lines = input.Replace(" ", "").Split("\r\n")

    let parse (line: string) =
        let l = lookup.[line[0]]
        let rEncrypted = line[1]
        let r = getCorrespondingMove l rEncrypted
        l, r

    let roundsPlayed =
        seq {
            for l in lines -> parse l
        }

    roundsPlayed |> Seq.map score |> Seq.sum



printfn "%i" (day2 input)
printfn "%i" (day2part2 input)