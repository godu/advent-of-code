namespace AdventOfCode

open System
module Day8 =
    let split (sep: string) (s: string): string seq = s.Split([|sep|], StringSplitOptions.None) |> Array.toSeq

    type Variable = string
    type Value = int
    type Modifier = Increment | Decrement
    type Comparator = Greater | GreaterOrEqual | Lower | LowerOrEqual | Equal | NotEqual

    type Condition = {
      Target: Variable;
      Comparator: Comparator;
      Value: Value;
    }
    type Instruction = {
      Target: Variable;
      Modifier: Modifier;
      Value: Value;
      Condition: Condition
    }

    type Register = Map<Variable, Value>
    let emptyRegister = Map.empty<Variable, Value>

    let parseModifier str =
      match str with
      | "inc" -> Some Increment
      | "dec" -> Some Decrement
      | _ -> None

    let parseComparator str =
      match str with
      | ">" -> Some Greater
      | ">=" -> Some GreaterOrEqual
      | "<" -> Some Lower
      | "<=" -> Some LowerOrEqual
      | "==" -> Some Equal
      | "!=" -> Some NotEqual
      | _ -> None

    let parseValue = (int)

    let parseLine line =
      match (line |> split " " |> Seq.toList) with
      | [a; b; c; _; d; e; f] ->
        match (a, parseModifier b, parseValue c, d, parseComparator e, parseValue f) with
        | (variable, Some modifier, modifierValue, conditionVariable, Some comparator, comparedValue) ->
          let condition = { Target = conditionVariable; Comparator = comparator; Value = comparedValue; }
          let instruction = { Target = variable; Modifier = modifier; Value = modifierValue; Condition = condition; }
          Some instruction
        | _ -> None
      | _ -> None
    let parseLines = Seq.map parseLine

    let parseInput =
      (split "\n") >> parseLines


    let matchCondition (condition: Condition) register =
      let value = register |> Map.tryFind condition.Target |> Option.defaultValue 0
      let op =
        match condition.Comparator with
        | Greater -> (>)
        | GreaterOrEqual -> (>=)
        | Lower -> (<)
        | LowerOrEqual -> (<=)
        | Equal -> (=)
        | NotEqual -> (<>)
      op value condition.Value

    let apply register instructionOption =
      match instructionOption with
      | Some instruction ->
        match register |> matchCondition instruction.Condition with
        | true ->
          let op =
            match instruction.Modifier with
            | Increment -> (+)
            | Decrement -> (-)

          let prevValue = Map.tryFind instruction.Target register |> Option.defaultValue 0
          Map.add instruction.Target (op prevValue instruction.Value) register
        | false -> register
      | None -> register

    let answer input =
      let instructions = parseInput input
      let register = Seq.fold apply emptyRegister instructions
      register |> Map.toSeq |> Seq.map snd |> Seq.fold max 0
