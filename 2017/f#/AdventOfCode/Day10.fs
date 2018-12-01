namespace AdventOfCode

module Day10 =
  let split (sep: char) (s: string): string seq = s.Split([|sep|]) |> Array.toSeq

  type SkipSize = int
  type Length = int
  type Position = int
  type Rope = int list
  type State = {
    SkipSize: SkipSize;
    Position: Position;
    Rope: Rope;
  }

  let createInitialState rope = { SkipSize = 0; Position = 0; Rope = rope;}

  let reposition position (rope: Rope): Rope =
    let length = List.length rope
    let index = (position % length + length) % length
    List.concat [|List.skip index rope; List.take index rope|]

  let twist length rope =
     rope
     |> List.take length
     |> List.rev
     |> (fun twisted ->
      List.concat [|twisted; List.skip length rope|]
     )

  let sumFirstAndSecond (rope: Rope): int =
    (List.item 0 rope) * (List.item 1 rope)

  let getRope state =
    state.Rope

  let next state length =
    let nextRope =
      state.Rope
      |> reposition state.Position
      |> twist length
      |> reposition (- state.Position)

    {
      SkipSize = state.SkipSize + 1;
      Position = state.Position + state.SkipSize + length;
      Rope = nextRope;
    }

  let answer (rope: Rope) (input: string) =
    input
    |> split ','
    |> Seq.map (int)
    |> Seq.fold next (createInitialState rope)
    |> getRope
    |> sumFirstAndSecond
