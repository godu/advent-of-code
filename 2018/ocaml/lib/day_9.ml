open Core_kernel
(* open Util *)

type marble = int
type marbles = marble list

let string_of_marble = Int.to_string
let string_of_marbles (marbles: marbles) =
  match marbles with
  | current :: rest -> 
    let (before_zero, after_zero) = rest |> List.split_while ~f:((<>) 0) in

    (before_zero |> List.map ~f:string_of_marble)
    |> (@) [(Printf.sprintf
               "(%d)" current)]
    |> (@) (after_zero |> List.map ~f:string_of_marble)
    |> String.concat ~sep:" "
  | _ -> ""
let%test_unit "string_of_marbles" =
  [%test_result: string] (
    [25; 10; 21; 5; 22; 11; 1; 12; 6; 13; 3; 14; 7; 15; 0; 16; 8; 17; 4; 18; 19; 2; 24; 20]
    |> string_of_marbles
  ) ~expect: "0 16 8 17 4 18 19 2 24 20 (25) 10 21 5 22 11 1 12 6 13 3 14 7 15";
  [%test_result: string] (
    [0; 16; 8; 17; 4; 18; 19; 2; 24; 20; 25; 10; 21; 5; 22; 11; 1; 12; 6; 13; 3; 14; 7; 15]
    |> string_of_marbles
  ) ~expect: "(0) 16 8 17 4 18 19 2 24 20 25 10 21 5 22 11 1 12 6 13 3 14 7 15"

let rec step_forward (step: int) seq =
  match step with
  | 0 -> seq
  | step when step < 0 -> 
    seq
    |> step_forward (step + (seq |> List.length))
  | _ -> (
      match seq with
      | head :: tail -> 
        tail @ [head]
        |> step_forward (step - 1)
      | _ -> seq
    )
let%test_unit "step_forward" =
  [%test_result: int list] (
    [0; 2; 1; 3]
    |> step_forward 3
  ) ~expect: [3; 0; 2; 1];
  [%test_result: int list] (
    [0]
    |> step_forward 3
  ) ~expect: [0];
  [%test_result: int list] (
    [0; 2; 1; 3]
    |> step_forward (-3)
  ) ~expect: [2; 1; 3; 0];

type player = int
type state = {
  round: int;
  last_round: int;
  players: player list;
  marbles: marbles;
}

let rec run_game (state: state): state =
  Printf.sprintf "round: %d" state.round |> print_endline;
  match state with
  | {players = []; _} -> state;
  | {round; last_round; _} when round > last_round -> state;
  | {round; marbles; players = current_player :: next_players; _} when (marbles <> []) && ((round % 23) = 0) -> (
      match marbles |> step_forward (-7) with
      | current_marble :: next_marbles ->
        {
          state with
          marbles = next_marbles;
          round = round + 1;
          players = next_players @ [current_player + round + current_marble]
        }
        |> run_game
      | _ -> {
          state with
          marbles = marbles |> step_forward 2 |> (@) [round];
          round = round + 1;
          players = next_players @ [current_player]
        }
    )
  | {round; marbles; players = current_player :: next_players; _} ->
    {
      state with
      marbles = marbles |> step_forward 2 |> (@) [round];
      round = round + 1;
      players = next_players @ [current_player]
    }
    |> run_game



let%test_unit "run_game" =
  [%test_result: int list * int list] (
    {
      round = 0;
      last_round = 4;
      players = [0; 0; 0; 0; 0;];
      marbles = [];
    }
    |> run_game
    |> (fun ({players; marbles; _}) -> (players, marbles))
  ) ~expect: ([0; 0; 0; 0; 0;], [4; 2; 1; 3; 0]);
  [%test_result: int list * int list] (
    {
      round = 0;
      last_round = 23;
      players = [0; 0; 0; 0; 0;];
      marbles = [];
    }
    |> run_game
    |> (fun ({players; marbles; _}) -> (players, marbles))
  ) ~expect: ([0; 0; 0; 0; 32;], [19; 2; 20; 10; 21; 5; 22; 11; 1; 12; 6; 13; 3; 14; 7; 15; 0; 16; 8; 17; 4; 18;])

let part_1 (player_count: int) (last_marble: marble): player =
  {
    round = 0;
    last_round = last_marble + 1;
    players = player_count |> List.init ~f:(fun _ -> 0);
    marbles = []
  }
  |> run_game
  |> (fun {players;_} -> 
      players
    )
  |> List.max_elt ~compare:Int.compare
  |> Option.value ~default:0
let%test_unit "part_1" =
  [%test_result: int] (
    part_1 10 1618
  ) ~expect: 8317;
  (* [%test_result: int] (
     part_1 13 7999
     ) ~expect: 146373; *)
  [%test_result: int] (
    part_1 17 1104
  ) ~expect: 2764;
  (* [%test_result: int] (
     part_1 21 6111
     ) ~expect: 54718; *)
  (* [%test_result: int] (
     part_1 30 5807
     ) ~expect: 37305 *)