open Core_kernel

let fold (initialValue: int) (frequencies: int list): int =
  List.fold_left ~init:initialValue ~f:(+) frequencies
let%test_unit _ =
  [%test_eq: int] (
    [ 1; 1; 1 ]
    |> fold 0
  ) 3;
  [%test_eq: int] (
    [ 1; 1; -2 ]
    |> fold 0
  ) 0;
  [%test_eq: int] (
    [ -1; -2; -3 ]
    |> fold 0
  ) (-6);

module Int_set =  Set.Make(Int)

let part_1 (frequencies: int list) =
  frequencies
  |> fold 0
let%test _ = [ 1; 1; 1; 1; 1; -2; -1; -2; -3 ] |> part_1 |> (=) (-3)

let part_2 (frequencies: int list) =
  frequencies
  |> Sequence.repeat
  |> Sequence.concat_map ~f: Sequence.of_list
  |> Sequence.append (Sequence.of_list [0])
  |>  Sequence.folding_map
    ~init: 0
    ~f: (fun acc current -> 
        let next = (+) acc current in
        (next, next)
      )
  |> Sequence.fold_until
    ~init:Int_set.empty
    ~f:(fun set value ->
        match Int_set.mem set value with
        | true -> Base.Continue_or_stop.Stop(value)
        | false -> Base.Continue_or_stop.Continue(Int_set.add set value)
      )
    ~finish: (fun _ -> 0)
let%test_unit _ =
  [%test_eq: int] (
    [ 1; (-1) ]
    |> part_2
  ) 0;
  [%test_eq: int] (
    [ 3; 3; 4; (-2); (-4) ]
    |> part_2
  ) 10;
  [%test_eq: int] (
    [ (-6); 3; 8; 5; (-6) ]
    |> part_2
  ) 5;
  [%test_eq: int] (
    [ 7; 7; (-2); (-7); (-4) ]
    |> part_2
  ) 14;
