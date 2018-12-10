open Core_kernel

let rec sum (entries: int list) =
  match entries with
  | child_count :: meta_count :: tail -> (
      match child_count with
      | 0 ->
        let (metas, tail) = List.split_n tail meta_count in
        (
          metas |> List.fold ~init:0 ~f:(+),
          tail
        )
      | _ ->
        let rec loop count entries = 
          match count with
          | 0 -> (0, entries)
          | _ -> 
            let (metasA, restA) = sum entries in
            let (metasB, restB) = loop 
                (count - 1)
                restA in
            (metasA + metasB, restB) in
        let (child_metas, tail) = loop child_count tail in
        let (metas, tail) = List.split_n tail meta_count in
        (
          metas |> List.fold ~init:child_metas ~f:(+),
          tail
        )

    )
  | _ -> (0, entries)

let%test _ =
  [0; 1; 99;]
  |> sum
  |> (=) (99, [])
let%test _ =
  [0; 1; 99;]
  |> sum
  |> (=) (99, [])
let%test _ =
  [2; 3; 0; 3; 10; 11; 12; 1; 1; 0; 1; 99; 2; 1; 1; 2;]
  |> sum
  |> (=) (138, [])

let part_1 (input: string): int =
  input
  |> String.split ~on:' '
  |> List.map ~f:Int.of_string
  |> sum
  |> fst
let%test _ =
  "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
  |> part_1
  |> (=) 138