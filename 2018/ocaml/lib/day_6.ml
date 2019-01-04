open Core_kernel

(* https://en.wikipedia.org/wiki/Voronoi_diagram *)

type cell = Empty | Point of int | Area of int

let string_of_cell (cell : cell) : string =
  match cell with
  | Empty -> "  .  "
  | Point id -> Printf.sprintf "P(%d)" id
  | Area id -> Printf.sprintf "A(%d)" id

type point = int * int

type board = cell array array

let point_of_string (s : string) : point =
  s |> String.split ~on:',' |> List.map ~f:String.strip
  |> fun parts ->
  (List.nth_exn parts 0 |> Int.of_string, List.nth_exn parts 1 |> Int.of_string)

let%test _ = "1, 6" |> point_of_string |> ( = ) (1, 6)

let%test _ = "-1, -6" |> point_of_string |> ( = ) (-1, -6)

let get_manhatan_distance ((xA, yA) : point) ((xB, yB) : point) : int =
  Int.abs (xB - xA) + Int.abs (yB - yA)

let%test _ = get_manhatan_distance (0, 0) (6, 6) |> ( = ) 12

let%test _ = get_manhatan_distance (6, 6) (0, 0) |> ( = ) 12

let%test _ = get_manhatan_distance (-6, 6) (0, 0) |> ( = ) 12

let%test _ = get_manhatan_distance (6, -6) (0, 0) |> ( = ) 12

let%test _ = get_manhatan_distance (0, 0) (-6, 6) |> ( = ) 12

let%test _ = get_manhatan_distance (0, 0) (6, -6) |> ( = ) 12

let find_borders (points : point list) : point =
  points
  |> List.reduce ~f:(fun (xA, yA) (xB, yB) -> (Int.max xA xB, Int.max yA yB))
  |> Base.Option.value ~default:(0, 0)

let%test _ =
  [(1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9)]
  |> find_borders
  |> ( = ) (8, 9)

let make_board (points : point list) : board =
  let right, bottom = points |> find_borders in
  Array.make_matrix ~dimx:(right + 1) ~dimy:(bottom + 1) Empty

let%test _ =
  [(0, 0); (1, 2)] |> make_board
  |> ( = ) [|[|Empty; Empty; Empty|]; [|Empty; Empty; Empty|]|]

let%test _ =
  [(1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9)]
  |> make_board
  |> ( = )
       [| [| Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty |]
        ; [| Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty |]
        ; [| Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty |]
        ; [| Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty |]
        ; [| Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty |]
        ; [| Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty |]
        ; [| Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty |]
        ; [| Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty |]
        ; [| Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty
           ; Empty |] |]

let assign_area (points : point list) (point : point) : cell =
  let closer_sorted_points =
    points
    |> List.mapi ~f:(fun index a -> (index, get_manhatan_distance a point))
    |> List.sort ~compare:(fun a b -> Int.compare (snd a) (snd b))
  in
  match closer_sorted_points with
  | (index, distance) :: _ when distance = 0 -> Point index
  | (indexA, distanceA) :: (_, distanceB) :: _ when distanceA <> distanceB ->
      Area indexA
  | _ -> Empty

let%test _ =
  (0, 0)
  |> assign_area [(1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9)]
  |> ( = ) (Area 0)

let%test _ =
  (2, 5)
  |> assign_area [(1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9)]
  |> ( = ) Empty

let%test _ =
  (1, 1)
  |> assign_area [(1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9)]
  |> ( = ) (Point 0)

let%test _ =
  let points = [(1, 1); (1, 6); (8, 3); (3, 4); (5, 5); (8, 9)] in
  points |> make_board
  |> Array.mapi ~f:(fun x row ->
         Array.mapi ~f:(fun y _ -> assign_area points (x, y)) row )
  |> ( = )
       [| [| Area 0
           ; Area 0
           ; Area 0
           ; Area 0
           ; Empty
           ; Area 1
           ; Area 1
           ; Area 1
           ; Area 1
           ; Area 1 |]
        ; [| Area 0
           ; Point 0
           ; Area 0
           ; Area 0
           ; Empty
           ; Area 1
           ; Point 1
           ; Area 1
           ; Area 1
           ; Area 1 |]
        ; [| Area 0
           ; Area 0
           ; Area 0
           ; Area 3
           ; Area 3
           ; Empty
           ; Area 1
           ; Area 1
           ; Area 1
           ; Area 1 |]
        ; [| Area 0
           ; Area 0
           ; Area 3
           ; Area 3
           ; Point 3
           ; Area 3
           ; Empty
           ; Empty
           ; Empty
           ; Empty |]
        ; [| Area 0
           ; Area 0
           ; Area 3
           ; Area 3
           ; Area 3
           ; Area 4
           ; Area 4
           ; Area 4
           ; Area 4
           ; Area 5 |]
        ; [| Empty
           ; Empty
           ; Area 4
           ; Area 4
           ; Area 4
           ; Point 4
           ; Area 4
           ; Area 4
           ; Area 4
           ; Area 5 |]
        ; [| Area 2
           ; Area 2
           ; Area 2
           ; Area 2
           ; Area 4
           ; Area 4
           ; Area 4
           ; Area 4
           ; Area 5
           ; Area 5 |]
        ; [| Area 2
           ; Area 2
           ; Area 2
           ; Area 2
           ; Area 2
           ; Area 4
           ; Area 4
           ; Area 5
           ; Area 5
           ; Area 5 |]
        ; [| Area 2
           ; Area 2
           ; Area 2
           ; Point 2
           ; Area 2
           ; Area 2
           ; Empty
           ; Area 5
           ; Area 5
           ; Point 5 |] |]

module Int_set = Set.Make (Int)
module Int_map = Map.Make (Int)

let pick_border_areas (board : board) : Int_set.t =
  let id_of_cell cell =
    match cell with
    | Point index -> Some index
    | Area index -> Some index
    | _ -> None
  in
  let top_border_cells = Array.nget board 0 in
  let bottom_border_cells = Array.last board in
  let left_border_cells =
    board |> Array.map ~f:(fun row -> Array.nget row 0)
  in
  let right_border_cells = board |> Array.map ~f:Array.last in
  [top_border_cells; bottom_border_cells; left_border_cells; right_border_cells]
  |> Array.concat |> Array.map ~f:id_of_cell |> Array.to_list
  |> List.filter ~f:Option.is_some
  |> Option.all |> Option.value ~default:[] |> Int_set.of_list

let%test _ =
  [| [| Area 0
      ; Area 0
      ; Area 0
      ; Area 0
      ; Empty
      ; Area 1
      ; Area 1
      ; Area 1
      ; Area 1
      ; Area 1 |]
   ; [| Area 0
      ; Point 0
      ; Area 0
      ; Area 0
      ; Empty
      ; Area 1
      ; Point 1
      ; Area 1
      ; Area 1
      ; Area 1 |]
   ; [| Area 0
      ; Area 0
      ; Area 0
      ; Area 3
      ; Area 3
      ; Empty
      ; Area 1
      ; Area 1
      ; Area 1
      ; Area 1 |]
   ; [| Area 0
      ; Area 0
      ; Area 3
      ; Area 3
      ; Point 3
      ; Area 3
      ; Empty
      ; Empty
      ; Empty
      ; Empty |]
   ; [| Area 0
      ; Area 0
      ; Area 3
      ; Area 3
      ; Area 3
      ; Area 4
      ; Area 4
      ; Area 4
      ; Area 4
      ; Area 5 |]
   ; [| Empty
      ; Empty
      ; Area 4
      ; Area 4
      ; Area 4
      ; Point 4
      ; Area 4
      ; Area 4
      ; Area 4
      ; Area 5 |]
   ; [| Area 2
      ; Area 2
      ; Area 2
      ; Area 2
      ; Area 4
      ; Area 4
      ; Area 4
      ; Area 4
      ; Area 5
      ; Area 5 |]
   ; [| Area 2
      ; Area 2
      ; Area 2
      ; Area 2
      ; Area 2
      ; Area 4
      ; Area 4
      ; Area 5
      ; Area 5
      ; Area 5 |]
   ; [| Area 2
      ; Area 2
      ; Area 2
      ; Point 2
      ; Area 2
      ; Area 2
      ; Empty
      ; Area 5
      ; Area 5
      ; Point 5 |] |]
  |> pick_border_areas
  |> Int_set.equal ([0; 1; 2; 5] |> Int_set.of_list)

let get_surfaces (board : board) : int Int_map.t =
  board |> Array.to_list |> Array.concat |> Array.to_list
  |> List.fold ~init:Int_map.empty ~f:(fun acc cell ->
         let increment_index v = match v with Some i -> i + 1 | None -> 1 in
         match cell with
         | Point id -> Int_map.update acc id ~f:increment_index
         | Area id -> Int_map.update acc id ~f:increment_index
         | _ -> acc )

let%test _ =
  let expected =
    [(0, 15); (1, 14); (2, 15); (3, 9); (4, 17); (5, 10)]
    |> Int_map.of_alist_exn
  in
  [| [| Area 0
      ; Area 0
      ; Area 0
      ; Area 0
      ; Empty
      ; Area 1
      ; Area 1
      ; Area 1
      ; Area 1
      ; Area 1 |]
   ; [| Area 0
      ; Point 0
      ; Area 0
      ; Area 0
      ; Empty
      ; Area 1
      ; Point 1
      ; Area 1
      ; Area 1
      ; Area 1 |]
   ; [| Area 0
      ; Area 0
      ; Area 0
      ; Area 3
      ; Area 3
      ; Empty
      ; Area 1
      ; Area 1
      ; Area 1
      ; Area 1 |]
   ; [| Area 0
      ; Area 0
      ; Area 3
      ; Area 3
      ; Point 3
      ; Area 3
      ; Empty
      ; Empty
      ; Empty
      ; Empty |]
   ; [| Area 0
      ; Area 0
      ; Area 3
      ; Area 3
      ; Area 3
      ; Area 4
      ; Area 4
      ; Area 4
      ; Area 4
      ; Area 5 |]
   ; [| Empty
      ; Empty
      ; Area 4
      ; Area 4
      ; Area 4
      ; Point 4
      ; Area 4
      ; Area 4
      ; Area 4
      ; Area 5 |]
   ; [| Area 2
      ; Area 2
      ; Area 2
      ; Area 2
      ; Area 4
      ; Area 4
      ; Area 4
      ; Area 4
      ; Area 5
      ; Area 5 |]
   ; [| Area 2
      ; Area 2
      ; Area 2
      ; Area 2
      ; Area 2
      ; Area 4
      ; Area 4
      ; Area 5
      ; Area 5
      ; Area 5 |]
   ; [| Area 2
      ; Area 2
      ; Area 2
      ; Point 2
      ; Area 2
      ; Area 2
      ; Empty
      ; Area 5
      ; Area 5
      ; Point 5 |] |]
  |> get_surfaces
  |> Int_map.equal ( = ) expected

let part_1 (input : string list) : int =
  let points = input |> List.map ~f:point_of_string in
  let board =
    points |> make_board
    |> Array.mapi ~f:(fun x row ->
           Array.mapi ~f:(fun y _ -> assign_area points (x, y)) row )
  in
  let areas = points |> List.mapi ~f:(fun i _ -> i) |> Int_set.of_list in
  let infinite_areas = board |> pick_border_areas in
  let finite_areas =
    areas
    |> Int_set.filter ~f:(fun area ->
           Int_set.exists ~f:(( = ) area) infinite_areas |> not )
  in
  let surfaces = board |> get_surfaces in
  finite_areas |> Int_set.to_list
  |> List.sort ~compare:(fun a b ->
         Option.compare
           (fun a b -> Int.compare a b |> Int.neg)
           (Int_map.find surfaces a) (Int_map.find surfaces b) )
  |> List.hd_exn |> Int_map.find surfaces |> Option.value ~default:0

let%test _ =
  ["1, 1"; "1, 6"; "8, 3"; "3, 4"; "5, 5"; "8, 9"] |> part_1 |> ( = ) 17
