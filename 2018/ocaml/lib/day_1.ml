let fold (initialValue: int) (frequencies: int list): int =
  List.fold_left (+) initialValue frequencies
let%test _ = 
  [ 1; 1; 1 ]
  |> fold 0
  |> (=) 3
let%test _ = 
  [ 1; 1; -2 ]
  |> fold 0
  |> (=) 0
let%test _ = 
  [ -1; -2; -3 ]
  |> fold 0
  |> (=) (-6)

module Int_set =  Set.Make(
  struct let compare = Pervasives.compare 
    type t = int
  end)

let part_1 (frequencies: int list) =
  frequencies
  |> fold 0
let%test _ = [ 1; 1; 1; 1; 1; -2; -1; -2; -3 ] |> part_1 |> (=) (-3)

let part_2 (frequencies: int list) =
  frequencies
  |> Base.Sequence.repeat
  |> Base.Sequence.concat_map ~f: Base.Sequence.of_list
  |> Base.Sequence.append (Base.Sequence.of_list [0])
  |>  Base.Sequence.folding_map
    ~init: 0
    ~f: (fun acc current -> 
        let next = (+) acc current in
        (next, next)
      )
  |> Base.Sequence.fold_until
    ~init: Int_set.empty
    ~f: (fun set value -> 
        if Int_set.mem value set
        then
          Base.Continue_or_stop.Stop(value)
        else 
          Base.Continue_or_stop.Continue(Int_set.add value set)
      )
    ~finish: (fun _ -> 0)
let%test _ = [ 1; (-1) ] |> part_2 |> (=) 0
let%test _ = [ 3; 3; 4; (-2); (-4) ] |> part_2 |> (=) 10
let%test _ = [ (-6); 3; 8; 5; (-6) ] |> part_2 |> (=) 5
let%test _ = [ 7; 7; (-2); (-7); (-4) ] |> part_2 |> (=) 14