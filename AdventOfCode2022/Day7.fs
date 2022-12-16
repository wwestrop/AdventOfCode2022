module mod_day7
open System.Linq
open System
open System.Collections.Generic
open System.IO

let mutable input = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"


input <- File.ReadAllText("input.txt");


let day7 (input: string) =

    let dirContents = Dictionary<string, uint64>()

    let mutable cwd: string list = []
    let getPath () =
        // It's backwards but for the sake of a dictionary key it doesn't matter (except it will if I rely on it for seeing a prefix, hmm)
        $"/{String.Join('/', cwd.Reverse())}"

    let interpretCd (cmd: string) =
        let cmd = cmd.Replace("$ ", "")
        let cmd::arg::_ = Seq.toList (cmd.Split(" "))         // Having to convert to F# list to get destructing is ugly. Is there a better way of doing it, while still using the .NET library types and functions we know and love?
        
        cwd <- match cmd, arg with
        | "cd", "/" -> []
        | "cd", ".." -> cwd.Tail
        | "cd", newDir -> newDir::cwd

        ()

    let sumOfThingsWithThisPreix (prefix: string) =
        dirContents.Keys
        |> Seq.filter (fun k -> k.StartsWith prefix)
        |> Seq.map (fun k -> dirContents[k])
        |> Seq.sum

    let getSizes () =
        dirContents.Keys
        |> Seq.map (fun x -> x, sumOfThingsWithThisPreix x)

    let incrementTotal size =
        let dir = getPath () 
        if not(dirContents.ContainsKey(dir)) then
            dirContents[dir] <- 0UL
            ()

        dirContents[dir] <- dirContents[dir] + size
        ()
    
    let countItemSize (cmd: string) =
        let (itemsize::_) = Seq.toList (cmd.Split(' '))     // Again, ugly on the requirement to convert type to get destructuring
        let itemsize = if itemsize = "dir" then 0UL else uint64 itemsize
        incrementTotal itemsize
        ()

    let interpretLine (line: string) =
        match line with
        | line when line.StartsWith("$ ls") -> ()
        | line when line.StartsWith("$ cd") -> interpretCd line
        | _ -> countItemSize line
        
        ()

    // A lovely side-effect mutating state, how very functional 😅
    let _ =
        input.Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map interpretLine
        |> Seq.toList

    getSizes ()
        |> Seq.map (fun (_, s) -> s)
        |> Seq.filter (fun x -> x < 100000UL)
        |> Seq.sum



printfn "%i" (day7 input)