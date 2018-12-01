namespace AdventOfCode
open System

module Day12 =
  let split (sep: string) (s: string): string seq = s.Split([|sep|], StringSplitOptions.None) |> Array.toSeq

  type Program = int
  type Pipes = Map<Program, Set<Program>>

  let toProgram: string -> Program = (int)

  let toPipes input: Pipes =
    input
    |> split "\n"
    |> Seq.map (fun line ->
      line
      |> split " <-> "
      |> (fun s ->
        ( s |> Seq.item 0 |> toProgram
        , s |> Seq.item 1 |> split ", " |> Seq.map toProgram |> Set.ofSeq )
      )
    )
    |> Map.ofSeq

  type State = {
    Programs: Set<Program>;
    ToExplore: Program list;
    Pipes: Pipes;
  }

  let explore state =
    match state.ToExplore with
    | head :: tail ->
      let toExplore =
        state.Pipes
        |> Map.find head
        |> Set.filter (fun p ->
          state.Programs
          |> Set.contains p
          |> not
        )
        |> Set.toList
      let newState = {
        state with
          Programs = Set.add head state.Programs;
          ToExplore = List.concat [|tail; toExplore|]
      }
      Some (newState, newState)
    | _ -> None

  let answer input =
    let pipes =
      input
      |> toPipes

    {
      Programs = Set.empty<Program>;
      ToExplore =
        pipes
        |> Map.tryFind 0
        |> Option.defaultValue Set.empty<Program>
        |> Set.toList;
      Pipes = pipes;
    }
    |> Seq.unfold explore
    |> Seq.last
    |> (fun state -> state.Programs)
    |> Set.count

