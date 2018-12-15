open Core_kernel
(* open Util *)

type marble = int
type marbles = marble Deque.t

(* let string_of_marble = Int.to_string
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
   ) ~expect: "(0) 16 8 17 4 18 19 2 24 20 25 10 21 5 22 11 1 12 6 13 3 14 7 15" *)

let deque_of_list (list: 'a list): 'a Deque.t =
  let deque = Deque.create() in
  list |> List.iter ~f:(fun el -> Deque.enqueue_back deque el);
  deque
let rec step_forward (step: int) seq =
  match Deque.is_empty seq with
  | true -> seq
  | false -> (
      match step with
      | step when step > 0 ->
        Deque.dequeue_front_exn seq
        |> Deque.enqueue_back seq;

        step_forward (step - 1) seq
      | step when step < 0 ->
        Deque.dequeue_back_exn seq
        |> Deque.enqueue_front seq;

        step_forward (step + 1) seq
      | _ -> seq
    )
let%test_unit "step_forward" =
  [%test_result: int list] (
    [ 0; 2; 1; 3 ]
    |> deque_of_list
    |> step_forward 3
    |> Deque.to_list
  ) ~expect: [ 3; 0; 2; 1 ];
  [%test_result: int list] (
    [ 0 ]
    |> deque_of_list
    |> step_forward 3
    |> Deque.to_list
  ) ~expect: [ 0 ];
  [%test_result: int list] (
    [ 0; 2; 1; 3 ]
    |> deque_of_list
    |> step_forward (-3)
    |> Deque.to_list
  ) ~expect: [ 2; 1; 3; 0 ];

type player = int
type state = {
  round: int;
  last_round: int;
  players: player list;
  marbles: marbles;
}

let rec run_game (state: state): state =
  match state with
  | {players = []; _} -> state;
  | {round; last_round; _} when round > last_round -> state;
  | {round; marbles; players = current_player :: next_players; _} when Deque.is_empty marbles |> not && ((round % 23) = 0) -> 
    let next_marbles = marbles |> step_forward (-7) in
    let current_marble = Deque.dequeue_front_exn marbles in
    {
      state with
      marbles = next_marbles;
      round = round + 1;
      players = next_players @ [current_player + round + current_marble]
    }
    |> run_game
  | {round; marbles; players = current_player :: next_players; _} ->
    let next_marbles = marbles |> step_forward 2 in
    Deque.enqueue_front next_marbles round;
    {
      state with
      round = round + 1;
      marbles = next_marbles;
      players = next_players @ [current_player]
    }
    |> run_game

let%test_unit "run_game" =
  [%test_result: int list * int list] (
    {
      round = 0;
      last_round = 4;
      players = [0; 0; 0; 0; 0;];
      marbles = Deque.create();
    }
    |> run_game
    |> (fun ({players; marbles; _}) -> (players, marbles |> Deque.to_list))
  ) ~expect: ([0; 0; 0; 0; 0;], [4; 2; 1; 3; 0]);
  [%test_result: int list * int list] (
    {
      round = 0;
      last_round = 23;
      players = [0; 0; 0; 0; 0;];
      marbles = Deque.create();
    }
    |> run_game
    |> (fun ({players; marbles; _}) -> (players, marbles |> Deque.to_list))
  ) ~expect: ([0; 0; 0; 0; 32;], [19; 2; 20; 10; 21; 5; 22; 11; 1; 12; 6; 13; 3; 14; 7; 15; 0; 16; 8; 17; 4; 18;])

let part_1 (player_count: int) (last_marble: marble): player =
  {
    round = 0;
    last_round = last_marble + 1;
    players = player_count |> List.init ~f:(fun _ -> 0);
    marbles = Deque.create()
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
  [%test_result: int] (
    part_1 13 7999
  ) ~expect: 146373;
  [%test_result: int] (
    part_1 17 1104
  ) ~expect: 2764;
  [%test_result: int] (
    part_1 21 6111
  ) ~expect: 54718;
  [%test_result: int] (
    part_1 30 5807
  ) ~expect: 37305