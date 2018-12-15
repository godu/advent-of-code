open Printf
open Advent_of_code

let main () =
  let input_day_1 =
    Input.day_1
    |> List.map int_of_string in
  input_day_1
  |> Day_1.part_1
  |> sprintf "Day1.1 : %d"
  |> print_endline;
  input_day_1
  |> Day_1.part_2
  |> sprintf "Day1.2 : %d"
  |> print_endline;

  Input.day_2
  |> Day_2.part_1
  |> sprintf "Day2.1 : %d"
  |> print_endline;
  Input.day_2
  |> Day_2.part_2
  |> sprintf "Day2.1 : %s"
  |> print_endline;

  Input.day_3
  |> Day_3.part_1 1000 1000
  |> sprintf "Day3.1 : %d"
  |> print_endline;
  Input.day_3
  |> Day_3.part_2 1000 1000
  |> sprintf "Day3.2 : %d"
  |> print_endline;

  Input.day_4
  |> Day_4.part_1
  |> sprintf "Day4.1 : %d"
  |> print_endline;
  Input.day_4
  |> Day_4.part_2
  |> sprintf "Day4.2 : %d"
  |> print_endline;

  Input.day_5
  |> Day_5.part_1
  |> sprintf "Day5.1 : %d"
  |> print_endline;
  Input.day_5
  |> Day_5.part_2
  |> sprintf "Day5.2 : %d"
  |> print_endline;

  Input.day_6
  |> Day_6.part_1
  |> sprintf "Day6.1 : %d"
  |> print_endline;

  Input.day_7
  |> Day_7.part_1
  |> sprintf "Day7.1 : %s"
  |> print_endline;
  Input.day_7
  |> Day_7.part_2 5 60
  |> sprintf "Day7.2 : %d"
  |> print_endline;

  Input.day_8
  |> Day_8.part_1
  |> sprintf "Day8.1 : %d"
  |> print_endline;
  Input.day_8
  |> Day_8.part_2
  |> sprintf "Day8.2 : %d"
  |> print_endline;

  Day_9.part_1 418 70769
  |> sprintf "Day9.1 : %d"
  |> print_endline;
  Day_9.part_1 418 7076900
  |> sprintf "Day9.2 : %d"
  |> print_endline

let _ = main()