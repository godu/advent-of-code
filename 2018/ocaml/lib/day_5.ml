open Core_kernel

let inverse_case c =
  match c |> Char.is_lowercase with
  | false -> c |> Char.lowercase
  | true -> c |> Char.uppercase

let react s =
  s
  |> String.to_list_rev
  |> List.fold ~init:[] ~f:(fun acc char -> 
      match acc with
      | head :: tail when head = (inverse_case char) -> tail
      | _ -> [char] @ acc
    )
  |> String.of_char_list


let%test _ = "aA" |> react = ""
let%test _ = "abBA" |> react = ""
let%test _ = "abAB" |> react = "abAB"
let%test _ = "aabAAB" |> react = "aabAAB"
let%test _ = "dabAcCaCBAcCcaDA" |> react = "dabCBAcaDA"

let part_1 input = input |> react |> String.length

let get_units input: (char * char) list =
  input
  |> String.lowercase
  |> String.to_list
  |> List.dedup_and_sort ~compare:Char.compare
  |> List.map ~f:(fun a -> (a |> Char.lowercase, a |> Char.uppercase))
let%test _ = 
  "dabAcCaCBAcCcaDA" 
  |> get_units 
  |> (=) [ ('a', 'A'); ('b', 'B'); ('c', 'C'); ('d', 'D'); ]

let generate_variant_polymers polymer =
  polymer
  |> get_units
  |> List.map ~f:(fun (l, u) -> 
      polymer
      |> String.to_list
      |> List.filter ~f:(fun c -> c <> l)
      |> List.filter ~f:(fun c -> c <> u)
      |> String.of_char_list
    )
let%test _ =
  "dabAcCaCBAcCcaDA"
  |> generate_variant_polymers
  |> (=) [
    "dbcCCBcCcD";
    "daAcCaCAcCcaDA";
    "dabAaBAaDA";
    "abAcCaCBAcCcaA";
  ]

let part_2 polymer =
  polymer
  |> generate_variant_polymers
  |> List.map ~f:react
  |> List.map ~f:String.length
  |> List.min_elt ~compare:Int.compare
  |> Base.Option.value ~default:0
let%test _ =
  "dabAcCaCBAcCcaDA"
  |> part_2
  |> (=) 4