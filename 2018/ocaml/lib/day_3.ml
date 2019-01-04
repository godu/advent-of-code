type claim = {id: int; top: int; left: int; width: int; height: int}

let claim_regexp = Re.Pcre.regexp "^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$"

let claim_of_string str =
  let matches = str |> Re.exec claim_regexp |> Re.get_all |> Array.to_list in
  match matches with
  | [_; id; left; top; width; height] ->
      Some
        { id= id |> int_of_string
        ; top= top |> int_of_string
        ; left= left |> int_of_string
        ; width= width |> int_of_string
        ; height= height |> int_of_string }
  | _ -> None

let print_claim claim =
  Printf.sprintf "Claim { id=%d; top=%d; left=%d; width=%d; height=%d; }"
    claim.id claim.top claim.left claim.width claim.height
  |> print_endline

let%test _ =
  "#123 @ 3,2: 5x4" |> claim_of_string
  = Some {id= 123; left= 3; top= 2; width= 5; height= 4}

let%test _ =
  "#1 @ 1,3: 4x4" |> claim_of_string
  = Some {id= 1; left= 1; top= 3; width= 4; height= 4}

let%test _ =
  "#2 @ 3,1: 4x4" |> claim_of_string
  = Some {id= 2; left= 3; top= 1; width= 4; height= 4}

let%test _ =
  "#3 @ 5,5: 2x2" |> claim_of_string
  = Some {id= 3; left= 5; top= 5; width= 2; height= 2}

type piece = int list array array

let make_piece row column = [] |> Array.make_matrix row column

let%test _ = make_piece 3 2 = [|[|[]; []|]; [|[]; []|]; [|[]; []|]|]

let mark_square (row : int) (column : int) (id : int) (piece : piece) : piece =
  let row = piece.(row) in
  let cell = row.(column) in
  row.(column) <- List.append cell [id] ;
  piece

let%test _ =
  make_piece 3 2 |> mark_square 2 1 3 = [|[|[]; []|]; [|[]; []|]; [|[]; [3]|]|]

let rec range i j = if i > j then [] else i :: range (i + 1) j

let%test _ = range 3 5 = [3; 4; 5]

let mark_claim piece (claims : claim list) =
  claims
  |> List.iter (fun claim ->
         piece
         |> Array.iteri (fun rowIndex row ->
                match
                  claim.top <= rowIndex && rowIndex < claim.top + claim.height
                with
                | true ->
                    row
                    |> Array.iteri (fun columnIndex _ ->
                           match
                             columnIndex >= claim.left
                             && columnIndex < claim.left + claim.width
                           with
                           | true ->
                               mark_square rowIndex columnIndex claim.id piece
                               |> ignore
                           | false -> () )
                | false -> () ) ) ;
  piece

let%test _ =
  ["#123 @ 3,2: 5x4"] |> List.map claim_of_string
  |> List.map Base.Option.to_list
  |> List.flatten
  |> mark_claim (make_piece 9 11)
  = [| [|[]; []; []; []; []; []; []; []; []; []; []|]
     ; [|[]; []; []; []; []; []; []; []; []; []; []|]
     ; [|[]; []; []; [123]; [123]; [123]; [123]; [123]; []; []; []|]
     ; [|[]; []; []; [123]; [123]; [123]; [123]; [123]; []; []; []|]
     ; [|[]; []; []; [123]; [123]; [123]; [123]; [123]; []; []; []|]
     ; [|[]; []; []; [123]; [123]; [123]; [123]; [123]; []; []; []|]
     ; [|[]; []; []; []; []; []; []; []; []; []; []|]
     ; [|[]; []; []; []; []; []; []; []; []; []; []|]
     ; [|[]; []; []; []; []; []; []; []; []; []; []|] |]

let%test _ =
  ["#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2"]
  |> List.map claim_of_string
  |> List.map Base.Option.to_list
  |> List.flatten
  |> mark_claim (make_piece 8 8)
  = [| [|[]; []; []; []; []; []; []; []|]
     ; [|[]; []; []; [2]; [2]; [2]; [2]; []|]
     ; [|[]; []; []; [2]; [2]; [2]; [2]; []|]
     ; [|[]; [1]; [1]; [1; 2]; [1; 2]; [2]; [2]; []|]
     ; [|[]; [1]; [1]; [1; 2]; [1; 2]; [2]; [2]; []|]
     ; [|[]; [1]; [1]; [1]; [1]; [3]; [3]; []|]
     ; [|[]; [1]; [1]; [1]; [1]; [3]; [3]; []|]
     ; [|[]; []; []; []; []; []; []; []|] |]

let part_1 row_count column_count input =
  let piece = make_piece row_count column_count in
  let claims =
    input |> List.map claim_of_string
    |> List.map Base.Option.to_list
    |> List.flatten
  in
  claims |> mark_claim piece
  |> Array.fold_left
       (fun count row ->
         row
         |> Array.map (fun cell -> if cell |> List.length > 1 then 1 else 0)
         |> Array.fold_left ( + ) 0 |> ( + ) count )
       0

let%test _ =
  ["#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2"] |> part_1 8 8 = 4

module Int_set = Set.Make (struct
  let compare = Pervasives.compare

  type t = int
end)

let part_2 row_count column_count input =
  let piece = make_piece row_count column_count in
  let claims =
    input |> List.map claim_of_string
    |> List.map Base.Option.to_list
    |> List.flatten
  in
  let ids = claims |> List.map (fun {id; _} -> id) |> Int_set.of_list in
  let overlaped_ids =
    claims |> mark_claim piece
    |> Array.fold_left
         (fun ids_set row ->
           row |> Array.to_list
           |> List.filter (fun cell -> cell |> List.length > 1)
           |> List.fold_left
                (fun ids_set cell ->
                  List.fold_left (fun set a -> Int_set.add a set) ids_set cell
                  )
                ids_set )
         Int_set.empty
  in
  Int_set.diff ids overlaped_ids |> Int_set.find_first (fun _ -> true)

let%test _ =
  ["#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2"] |> part_2 8 8 = 3
