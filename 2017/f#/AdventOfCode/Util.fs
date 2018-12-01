namespace AdventOfCode

module Util =
    open System
    let split (sep: string) (s: string): string seq = s.Split([|sep|], StringSplitOptions.None) |> Array.toSeq

