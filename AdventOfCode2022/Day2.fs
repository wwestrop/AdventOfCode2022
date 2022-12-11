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


let day2 (input: string) =

    let lines = input.Replace(" ", "").Split("\r\n")

    let parse (line: string) =
        lookup.[line[0]], lookup.[line[1]]

    let roundsPlayed =
        seq {
            for l in lines -> parse l
        }

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

    roundsPlayed |> Seq.map score |> Seq.sum



printfn "%i" (day2 input)


// 13809