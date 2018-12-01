namespace AdventOfCode

module Day4 =
    let split (sep: char) (s: string): string[] = s.Split [|sep|]

    let answer (input: string): int =
        input
        |> split '\n'
        |> Seq.map (split ' ')
        |> Seq.filter (fun row ->
            row
            |> (fun words -> Seq.length words = Set.count (Set.ofArray words))
        )
        |> Seq.length