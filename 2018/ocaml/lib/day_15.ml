(* open Core_kernel
open Util

module Coordinate = struct
  type t = int * int

  let compare (xa, ya) = function
    | (xb, yb) when xa < xb || ya < yb -> 1
    | (xb, yb) when xa > xb || ya > yb -> -1
    | _ -> 0
  let%test _ = compare (1, 1) (0, 1) = -1
  let%test _ = compare (1, 1) (1, 0) = -1
  let%test _ = compare (1, 1) (1, 2) = 1
  let%test _ = compare (1, 1) (2, 1) = 1
  let%test _ = compare (1, 1) (1, 1) = 0

  let min a b = match compare a b with
    | 1 -> a
    | _ -> b
  let%test _ = min (1, 1) (0, 1) = (0, 1)
  let%test _ = min (1, 1) (1, 0) = (1, 0)
  let%test _ = min (1, 1) (1, 2) = (1, 1)
  let%test _ = min (1, 1) (2, 1) = (1, 1)

  let get_neigbords ((x, y): t): t list =
    [
      (x - 1, y);
      (x, y - 1);
      (x, y + 1);
      (x + 1, y);
    ]
end

module Plan = struct
  module Cell = struct
    type t =
      | Elf of int
      | Goblin of int
      | Wall
      | Cavern

    let is_opponent a b = match (a, b) with
      | (Elf(_), Goblin(_)) -> true
      | (Goblin(_), Elf(_)) -> true
      | _ -> false

    let to_char = function
      | Wall -> '#'
      | Cavern -> '.'
      | Elf(_) -> 'E'
      | Goblin(_) -> 'G'

    let of_char = function
      | '#' -> Some Wall
      | '.' -> Some Cavern
      | 'E' -> Some (Elf 200)
      | 'G' -> Some (Goblin 200)
      | _ -> None

    let get_hit_point = function
      | Elf(hit_point) -> hit_point
      | Goblin(hit_point) -> hit_point
      | _ -> Int.max_value

    let compare a b = Int.compare (get_hit_point a) (get_hit_point b)
  end
  type t = Cell.t array array

  let map ~f = Array.map ~f:(Array.map ~f:f)
  let foldi ~f ~init =
    Array.foldi
      ~init:init
      ~f:(fun x init ->
          Array.foldi
            ~init:init
            ~f:(fun y -> f (x, y))
        )

  let pick ~f = foldi
      ~init:[]
      ~f:(fun position acc cell-> match f cell with
          | true -> [(position, cell)] @ acc
          | _ -> acc
        ) >> List.rev

  let of_string =
    String.strip >> String.split ~on:'\n' >> List.map ~f:(
      String.to_list >> List.map ~f:Cell.of_char >> Option.all
    ) >> Option.all >> Option.map ~f:(Array.of_list_map ~f:Array.of_list)



  let to_string =
    List.map ~f:(
      List.map ~f:(
        Cell.to_char >> Char.to_string
      ) >> String.concat
    ) >> String.concat ~sep:"\n"

  let get_cell plan (x, y) =
    try Some (Array.get plan x |> fun row -> Array.get row y)
    with Invalid_argument(_) -> None
  let set_cell plan (x, y) cell =
    let next_plan = Array.copy plan in
    try Some (
      Array.get next_plan x
      |> fun row -> Array.set row y cell
      |> ignore;
      next_plan
    )
    with Invalid_argument(_) -> None
end


module Game = struct
  type attack_power = {elf: int; goblin: int}
  type state = {
    plan: Plan.t;
    round: int;
    next_units: Coordinate.t list;
    attack_power: attack_power;
  }

  let pick_units_position =
    let open Plan in
    let open Plan.Cell in
    pick ~f:(function
        | Elf(_) -> true
        | Goblin(_) -> true
        | _ -> false
      ) >> List.map ~f:fst
  let%test _ =
    let open Plan.Cell in
    pick_units_position [|
      [| Wall; Elf 200; Wall; |];
      [| Goblin 200; Elf 200; Goblin 200; |];
      [| Wall; Goblin 200; Wall; |];
    |]
    |> (=) [(0, 1);(1, 0);(1, 1);(1, 2);(2, 1);]


  let make_state ?(plan=[||]) ?(round=0) ?(attack_power={elf=3;goblin=3;}) ?(next_units=[]) _: state =
    { plan=plan; round=round; attack_power=attack_power; next_units=next_units; };;


  let attack state: state option =
    match state with
    | {next_units = current_unit :: _; plan; attack_power; _} ->
      let open Option in
      let enemies = Plan.get_cell plan current_unit
       |> Option.map ~f:(fun cell ->
          Coordinate.get_neigbords current_unit
          |> List.filter ~f:(fun position ->
            position
            |> Plan.get_cell plan
            |> Option.value_map ~default:false ~f:(Plan.Cel l.is_opponent cell)
          )
          |> List.sort ~compare:(fun a b ->
            Option.compare
              Plan.Cell.compare
              (Plan.get_cell plan a)
              (Plan.get_cell plan b)
          )

          |> List.hd
          |> Option.map ~f:(fun opponent_position ->
            let open Plan.Cell in
            Plan.get_cell plan opponent_position
            |> Option.map ~f:(function
              | Elf(hit_point) when hit_point > 3 -> Elf(hit_point - attack_power.goblin)
              | Goblin(hit_point) when hit_point > 3 -> Goblin(hit_point - attack_power.elf)
              | _ -> Cavern
            )
            |> Option.map ~f:(Plan.set_cell plan opponent_position)
          )
      ) in
      let target = enemies Option.(>>=) List.hd in
      None
    | _ -> None

  let%test _ =
    let open Plan.Cell in
    let (state, next_state) = (
      {(make_state [|
        [| Goblin 200; Elf 200; Goblin 200; |];
      |]) with next_units = [(0, 1)]},
      {(make_state [|
        [| Goblin 197; Elf 200; Goblin 200; |];
      |]) with next_units = [(0, 1)]}
    ) in
    (attack state) = (Some next_state)
  let%test _ =
    let open Plan.Cell in
    let (state, next_state) = (
      {(make_state [|
        [| Goblin 200; Elf 200; Goblin 197; |];
      |]) with next_units = [(0, 1)]},
      {(make_state [|
        [| Goblin 200; Elf 200; Goblin 194; |];
      |]) with next_units = [(0, 1)]}
    ) in
    (attack state) = (Some next_state)
  let%test _ =
    let open Plan.Cell in
    let (state, next_state) = (
      {(make_state [|
        [| Goblin 3; Elf 200; |];
      |]) with next_units = [(0, 1); (0, 0);]},
      {(make_state [|
        [| Cavern; Elf 200; |];
      |]) with next_units = [(0, 1)]}
    ) in
    (attack state) = (Some next_state)

  (* let move (state: state): state option = Some state

  let unfold (state: state): (state * state) option =
  let unfold = function
    | {next_units = []; _} -> None
    | {plan; _} when plan = [||] -> None
    | state -> Some (state, state)
      let next_state = state |> attack |> move in
         Some (next_state, next_state) *)
end *)
