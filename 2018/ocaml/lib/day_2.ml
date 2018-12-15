open Core_kernel
open Util

module String_map = Map.Make(String)

let process_word (word: string) =
  word
  |> String.to_list
  |> Sequence.of_list
  |> Sequence.fold
    ~init:String_map.empty
    ~f:(fun map char ->
        String_map.update
          ~f:(fun (v) ->
              match v with
              | Some count -> count + 1
              | None -> 1
            )
          map
          (String.make 1 char)
      )
  |> (fun map -> 
      let hasExactlyTwo = map |> String_map.exists ~f:(fun count -> count = 2)   in
      let hasExactlyThree = map |> String_map.exists ~f:(fun count -> count = 3) in

      ( 
        hasExactlyTwo,
        hasExactlyThree
      )
    )
let%test_unit _ =
  [%test_eq: bool * bool] (
    "abcdef"
    |> process_word
  ) (false, false);
  [%test_eq: bool * bool] (
    "bababc"
    |> process_word
  ) (true, true);
  [%test_eq: bool * bool] (
    "abbcde"
    |> process_word
  ) (true, false);
  [%test_eq: bool * bool] (
    "abcccd"
    |> process_word
  ) (false, true);
  [%test_eq: bool * bool] (
    "aabcdd"
    |> process_word
  ) (true, false);
  [%test_eq: bool * bool] (
    "abcdee"
    |> process_word
  ) (true, false);
  [%test_eq: bool * bool] (
    "ababab"
    |> process_word
  ) (false, true)

let int_of_bool bool =
  match bool with
  | true -> 1
  | false -> 0

let process_words (words: string list) =
  words
  |> List.map ~f:process_word
  |> List.fold
    ~init:(0, 0)
    ~f:(fun (hasExactlyTwoCount, hasExactlyThreeCount) (hasExactlyTwo, hasExactlyThree) ->
        (
          (hasExactlyTwo |> int_of_bool |> (+) hasExactlyTwoCount),
          (hasExactlyThree |> int_of_bool |> (+) hasExactlyThreeCount)
        )
      )
  |> fun (hasExactlyTwoCount, hasExactlyThreeCount) -> hasExactlyTwoCount * hasExactlyThreeCount
let%test_unit _ =
  [%test_eq: int] (
    [ "abcdef"; "bababc"; "abbcde"; "abcccd"; "aabcdd"; "abcdee"; "ababab" ]
    |> process_words
  ) 12

let part_1 (words: string list): int =
  words
  |> process_words

let list_of_string = String.to_list >> List.map ~f:(String.make 1)

let compare_two_words a b =
  (list_of_string b) 
  |> List.map2_exn ~f:(fun a b ->
      if a = b
      then (0, a)
      else (1, "")
    ) (list_of_string a)
  |> List.fold
    ~init:(0, "")
    ~f:(fun (countA, charA) (countB, charB) ->
        (countA + countB, charA ^ charB)
      )
let%test_unit _ =
  [%test_eq: int * string] (
    compare_two_words "abcde" "axcye"
  ) (2, "ace");
  [%test_eq: int * string] (
    compare_two_words "fghij" "fguij"
  ) (1, "fgij")

let compare_each_words (words: string list) =
  Sequence.unfold
    ~init: words
    ~f: (fun state -> 
        match state with
        | current_word :: next_state ->
          Some (
            next_state |> List.map ~f:(compare_two_words current_word),
            next_state
          )
        | _ -> None
      )
  |> Sequence.to_list
  |> List.concat
let%test_unit _ =
  [%test_eq: (int * string) list] (
    ["fghij"; "fguij"]
    |> compare_each_words
  ) [(1, "fgij")];
  [%test_eq: (int * string) list] (
    ["abcde"; "fghij"; "fguij"]
    |> compare_each_words
  ) [(5, ""); (5, ""); (1, "fgij")]

let take_most_closer (diffs: (int * string) list): string = 
  diffs
  |> List.fold
    ~init:(Int.max_value, "")
    ~f:(fun (diffCountA, commonA) (diffCountB, commonB) ->
        if diffCountA > diffCountB
        then (diffCountB, commonB)
        else (diffCountA, commonA)
      )
  |> snd
let%test _ = take_most_closer [(5, ""); (5, ""); (1, "fgij")] = "fgij"
let%test_unit _ =
  [%test_eq: string] (
    [(5, ""); (5, ""); (1, "fgij")]
    |> take_most_closer
  ) "fgij"

let part_2 (words: string list) =
  words
  |> compare_each_words
  |> take_most_closer