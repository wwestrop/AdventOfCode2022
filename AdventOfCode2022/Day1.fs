open System.IO

let mutable input = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"


input <- File.ReadAllText("input.txt");


let day1 (input: string) =          // TODO parens enable type hint. Does it also create a tuple?
    let lines = input.Split("\r\n")
    
    let getElves =
        let mutable acc = 0         // hmmmmm, mutable...........
        seq {
            for l in lines do
                match l with
                | "" -> yield acc; acc <- 0         // this assumes the file ends with a blank line
                | x -> acc <- acc + int x
        }

    let elvesCalories = getElves
    let topElvesCalories = Seq.sortDescending elvesCalories |> Seq.take 3
    Seq.sum topElvesCalories



printfn "%i" (day1 input)