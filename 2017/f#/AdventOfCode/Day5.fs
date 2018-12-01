namespace AdventOfCode

module Day5 =
    let parseInt: string -> int = int
    let split (sep: char) (s: string): string[] = s.Split [|sep|]
    
    type State = { Position: int; Board: Map<int, int>}

    let step { Position= at; Board= board } =
        board
        |> Map.tryFind at
        |> Option.map (fun offset -> (), ({Position= at + offset; Board= Map.add at (offset+1) board}))
    
    let answer (input: string): int =
        let initialState = {
            Position= 0
            Board= input
                |> split '\n'
                |> Seq.map (parseInt)
                |> Seq.mapi (fun i v -> i, v)
                |> Map.ofSeq
        }

        initialState 
        |> Seq.unfold step 
        |> Seq.length
