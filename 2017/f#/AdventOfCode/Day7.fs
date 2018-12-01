namespace AdventOfCode

open System
module Day7 =
    let split (sep: string) (s: string): string seq = s.Split([|sep|], StringSplitOptions.None) |> Array.toSeq

    let getNodeName line = line |> split " " |> Seq.head

    let getChildren line =
      line
      |> split " -> "
      |> Seq.tryItem 1
      |> Option.map (split ", ")
      |> Option.defaultValue Seq.empty<string>


    let parseLine = (fun line ->
      (getNodeName line, getChildren line)
    )

    let parseLines = Seq.map parseLine

    let parseInput = split "\n" >> parseLines

    let getParentLine lines line =
      let node = fst line
      Seq.tryFind (snd >> Seq.contains node) lines
      |> Option.isSome
      |> not


    let answer input =
      let lines = parseInput input

      lines
      |> Seq.tryFind (getParentLine lines)
      |> Option.map fst
      |> Option.defaultValue (lines |> Seq.head |> fst)
