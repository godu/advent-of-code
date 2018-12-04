open Printf
open Advent_of_code

let main () =
  let input_day_1 =
    Input.day_1
    |> String.split_on_char '|'
    |> List.map int_of_string in
  input_day_1
  |> Day_1.part_1
  |> sprintf "Day1.1 : %d"
  |> print_endline;
  input_day_1
  |> Day_1.part_2
  |> sprintf "Day1.2 : %d"
  |> print_endline;

  let input_day_2 = Input.day_2 |> String.split_on_char '|' in
  input_day_2
  |> Day_2.part_1
  |> sprintf "Day2.1 : %d"
  |> print_endline;
  input_day_2
  |> Day_2.part_2
  |> sprintf "Day2.1 : %s"
  |> print_endline;

  let input_day_3 = Input.day_3 |> String.split_on_char '|' in
  input_day_3
  |> Day_3.part_1 1000 1000
  |> sprintf "Day3.1 : %d"
  |> print_endline;
  input_day_3
  |> Day_3.part_2 1000 1000
  |> sprintf "Day3.2 : %d"
  |> print_endline;

  let input_day_4 = Input.day_4 |> String.split_on_char '|' in
  input_day_4
  |> Day_4.part_1
  |> sprintf "Day4.1 : %d"
  |> print_endline

let _ = main()