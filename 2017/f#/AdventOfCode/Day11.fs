namespace AdventOfCode

module Day11 =
  let split (sep: char) (s: string): string seq = s.Split([|sep|]) |> Array.toSeq

  type Direction = NW | N | NE | SW | S | SE

  type Count = int
  type State = {
    NW: Count;
    N: Count;
    NE: Count;
    SW: Count;
    S: Count;
    SE: Count;
  }

  let toDirection str =
    match str with
    | "nw" -> Some NW
    | "n" -> Some N
    | "ne" -> Some NE
    | "sw" -> Some SW
    | "s" -> Some S
    | "se" -> Some SE
    | _ -> None

  let emptyState = { NW = 0; N = 0; NE = 0; SW = 0; S = 0; SE = 0}

  let reducer state direction =
    match direction with
    | Some NW -> { state with NW = state.NW + 1 }
    | Some N -> { state with N = state.N + 1 }
    | Some NE -> { state with NE = state.NE + 1 }
    | Some SW -> { state with SW = state.SW + 1 }
    | Some S -> { state with S = state.S + 1 }
    | Some SE -> { state with SE = state.SE + 1 }
    | _ -> state

  let sumStep state =
    state.NW + state.N + state.NE + state.SW + state.S + state.SE

  let simplify state =
    match state with
    | state when state.N > 0 && state.S > 0 ->
      Some {
        state with
          N = state.N - 1;
          S = state.S - 1;
      }
    | state when state.NW > 0 && state.SE > 0 ->
      Some {
        state with
          NW = state.NW - 1;
          SE = state.SE - 1;
      }
    | state when state.NE > 0 && state.SW > 0 ->
      Some {
        state with
          NE = state.NE - 1;
          SW = state.SW - 1;
      }
    | state when state.N > 0 && state.SW > 0 ->
      Some {
        state with
          NW = state.NW + 1;
          N = state.N - 1;
          SW = state.SW - 1;
      }
    | state when state.NW > 0 && state.NE > 0 ->
      Some {
        state with
          N = state.N + 1;
          NW = state.NW - 1;
          NE = state.NE - 1;
      }
    | state when state.N > 0 && state.SE > 0 ->
      Some {
        state with
          NE = state.NE + 1;
          N = state.N - 1;
          SE = state.SE - 1;
      }
    | state when state.NW > 0 && state.S > 0 ->
      Some {
        state with
          SW = state.SW + 1;
          NW = state.NW - 1;
          S = state.S - 1;
      }
    | state when state.SE > 0 && state.SW > 0 ->
      Some {
        state with
          S = state.S + 1;
          SE = state.SE - 1;
          SW = state.SW - 1;
      }
    | state when state.S > 0 && state.NE > 0 ->
      Some {
        state with
          SE = state.SE + 1;
          S = state.S - 1;
          NE = state.NE - 1;
      }
    | _ -> None
    |> Option.map (fun v -> (v, v))

  let answer (input: string) =
    input
    |> split ','
    |> Seq.map toDirection
    |> Seq.fold reducer emptyState
    |> (fun s ->
      s
      |> Seq.unfold simplify
      |> Seq.tryLast
      |> Option.defaultValue s
    )
    |> sumStep
