module Frequency =
struct
  type t = int

  let id_element = 0
  let compare = compare

  let combine = (+)
  let%test _ = combine 0 1 = 1
  let%test _ = combine 1 (-2) = (-1)
  let%test _ = combine (-1) 3 = 2
  let%test _ = combine 2 1 = 3

  let fold (initialValue: t) (frequencies: t list): t =
    List.fold_left combine initialValue frequencies
  let%test _ = 
    [ 1; 1; 1 ]
    |> fold id_element
    |> (=) 3
  let%test _ = 
    [ 1; 1; -2 ]
    |> fold id_element
    |> (=) 0
  let%test _ = 
    [ -1; -2; -3 ]
    |> fold id_element
    |> (=) (-6)
end

module FrequencySet = Set.Make(Frequency)


let part_1 (frequencies: Frequency.t list) =
  frequencies
  |> Frequency.fold Frequency.id_element
let%test _ = [ 1; 1; 1; 1; 1; -2; -1; -2; -3 ] |> part_1 |> (=) (-3)

let part_2 (frequencies: Frequency.t list) =
  frequencies
  |> Base.Sequence.repeat
  |> Base.Sequence.concat_map ~f: Base.Sequence.of_list
  |> Base.Sequence.append (Base.Sequence.of_list [Frequency.id_element])
  |>  Base.Sequence.folding_map
    ~init: 0
    ~f: (fun acc current -> 
        let next = Frequency.combine acc current in
        (next, next)
      )
  |> Base.Sequence.fold_until
    ~init: FrequencySet.empty
    ~f: (fun set value -> 
        if FrequencySet.mem value set
        then
          Base.Continue_or_stop.Stop(value)
        else 
          Base.Continue_or_stop.Continue(FrequencySet.add value set)
      )
    ~finish: (fun _ -> 0)

let%test _ = [ 1; (-1) ] |> part_2 |> (=) 0
let%test _ = [ 3; 3; 4; (-2); (-4) ] |> part_2 |> (=) 10
let%test _ = [ (-6); 3; 8; 5; (-6) ] |> part_2 |> (=) 5
let%test _ = [ 7; 7; (-2); (-7); (-4) ] |> part_2 |> (=) 14