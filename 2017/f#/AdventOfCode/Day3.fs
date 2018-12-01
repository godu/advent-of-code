namespace AdventOfCode

module Day3 =
    type Postition = { Horizontal: int; Vertical: int }
    type Direction = Left | Right | Up | Down
    type State = { At: Postition; Facing: Direction }

    let initialState = {
        At= {
            Horizontal= 0
            Vertical= 0
        }
        Facing= Right
    }
    
    let turn (state: State) =
        { state with
            Facing =
                match state.Facing with
                | Up -> Left
                | Left -> Down
                | Down -> Right
                | Right -> Up
        }

    let forward (state: State) =
        { state with
            At =
                match state.Facing with
                | Up -> { state.At with Vertical = state.At.Vertical + 1 }
                | Left -> { state.At with Horizontal = state.At.Horizontal - 1 }
                | Down -> { state.At with Vertical = state.At.Vertical - 1 }
                | Right -> { state.At with Horizontal = state.At.Horizontal + 1 }
        }

    let (|TurningCorner|NonTurn|) {Horizontal = h; Vertical = v} =
        match h, v with
        | 0, 0 -> NonTurn
        | _ when h = -v && v < 0 -> NonTurn
        | _ when -h = v - 1 && v < 0 -> TurningCorner
        | _ when abs h = abs v -> TurningCorner
        | _ -> NonTurn

    let step (state: State) =
        match state.At with
        | TurningCorner -> state |> turn |> forward
        | NonTurn -> state |> forward

    let elementAt n =
        let rec aux n state =
            match n with
            | 1 -> state
            | _ -> aux (n - 1) (step state)

        match n with
        | 1 -> initialState
        | _ -> aux (n - 1) (turn (forward initialState))

    let answer (input: int): int =
        let state = elementAt input
        abs state.At.Horizontal + abs state.At.Vertical