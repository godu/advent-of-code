namespace AdventOfCode

module Day6 =
    let parseInt: string -> int = int
    let split (sep: char) (s: string): string[] = s.Split [|sep|]

    type State = { Seen: string Set; Bank: Map<int, int> }

    let rec redistribution i n bank =
        let nextI = (i + 1) % Map.count bank;

        if n = 0
        then bank
        else
            let value = Map.find nextI bank;
            let nextBank = Map.add nextI (value + 1) bank
            redistribution nextI (n - 1) nextBank

    let step { Seen= seen; Bank= bank } =
        let maxId, max = Seq.maxBy snd (Map.toSeq bank)
        let nextBank =
            bank
            |> Map.add maxId 0
            |> redistribution maxId max

        let key =
            bank
            |> Map.toSeq
            |> Seq.map (snd >> string)
            |> String.concat ","

        if seen.Contains key
        then None
        else Some (key, {Seen= Set.add key seen; Bank= nextBank})


    let printSeq seq =
        printfn "goo\n"
        for x in seq do
            printfn "%s" x
        seq

    let answer (input: string): int =
        let bank =
            input
            |> split '\t'
            |> Seq.mapi (fun i v -> i, parseInt v)
            |> Map.ofSeq

        let initialState = {Seen=Set.empty; Bank= bank}

        initialState
        |> Seq.unfold step
        |> printSeq
        |> Seq.length

