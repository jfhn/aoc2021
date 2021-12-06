open System.IO
open System

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type N = int64

let inputStrArray = ((Seq.toList (readLines "input.txt")).Head).Split ','

let input : N list = List.map (fun s -> Int64.Parse s) (Array.toList inputStrArray)

let newFishes list : N list = List.mapi (fun i v -> if i = 6 then v + (List.last list) else v) list

let nextDay list : N list = (List.tail list) @ [(List.head list)] |> newFishes

let rec simulate fishies maxDays day : N =
    if day = maxDays then
        List.sum fishies
    else
        simulate (nextDay fishies) maxDays (day + 1)

let rec fishieList (input: N list) (result: N list) : N list =
    match input, result with
    | [] , result -> result
    | lst, result -> fishieList (List.tail lst) (List.mapi (fun i v -> if int64(i) = (List.head lst) then v + 1L else v) result)

let solve1 input : N = simulate (fishieList input [0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L]) 80 0
printfn "%A" (solve1 input)

let solve2 input : N = simulate (fishieList input [0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L]) 256 0
printfn "%A" (solve2 input)
