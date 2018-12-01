namespace AdventOfCode

module Day1 =
    let parseInt: char -> int = string >> int

    let foo char1 char2 =
        if (char1 = char2)
        then (int (char1.ToString()))
        else 0

    let ifHeadIsLast s conc alt x =
        match Seq.tryHead s, Seq.tryLast s with
        | Some h, Some l -> if h = l then conc h x else alt x
        | _ -> alt x

    let answer (captcha:string): int =
        captcha
        |> Seq.map parseInt
        |> Seq.pairwise
        |> Seq.sumBy (fun (a, b) -> if a = b then a else 0)
        |> ifHeadIsLast (Seq.map parseInt captcha) (+) id
