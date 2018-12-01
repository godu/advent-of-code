namespace AdventOfCode

module Day9 =
  type Depth = int
  type Garbage = bool
  type Escape = bool
  type Score = int

  type State = {
    Depth: Depth
    Garbage: Garbage
    Escape: Escape
    Score: Score
  }

  let initialState = {
    Depth = 0;
    Garbage = false;
    Escape = false;
    Score = 0;
  }

  let walk (state: State) (char: char): State =
    match char,state with
    | (_, state) when state.Escape -> {state with Escape = false}
    | ('!', state) when state.Garbage && not state.Escape -> {state with Escape = true}
    | ('>', state) when state.Garbage -> {state with Garbage = false}
    | (_, state) when state.Garbage -> state
    | ('<', _) -> {state with Garbage = true}
    | ('}', _) -> {state with Depth = state.Depth - 1; Score = state.Score + state.Depth}
    | ('{', _) -> {state with Depth = state.Depth + 1}
    | _ -> state

  let answer (input: string): int =
    input
    |> Seq.fold walk initialState
    |> (fun s -> s.Score)
