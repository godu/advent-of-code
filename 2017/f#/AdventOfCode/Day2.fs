namespace AdventOfCode

module Day2 =
    let split (sep: char) (s: string): string[] = s.Split [|sep|]
    let parseInt: string -> int = (int)

    let answer (puzzle: string): int =
        puzzle
        |> (split '\n' >> Seq.map (split '\t' >> Seq.map parseInt))
        |> Seq.sumBy ((fun v -> (Seq.min v, Seq.max v)) >> (fun (min, max) -> max - min))