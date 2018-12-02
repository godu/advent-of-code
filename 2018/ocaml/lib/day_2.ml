module String_map = Map.Make(String)

let process_word (word: string) =
  word
  |> String.to_seq
  |> Seq.fold_left
    (fun map char -> 
       String_map.update
         (String.make 1 char)
         (fun (v) -> 
            match v with
            | Some count -> Some (count + 1)
            | None -> Some 1
         )
         map
    )
    String_map.empty
  |> (fun map -> 
      let hasExactlyTwo = String_map.exists (fun _ count -> count = 2) map  in
      let hasExactlyThree = String_map.exists (fun _ count -> count = 3) map in

      ( 
        hasExactlyTwo,
        hasExactlyThree
      )
    )

let%test _ = process_word "abcdef" = (false, false)
let%test _ = process_word "bababc" = (true, true)
let%test _ = process_word "abbcde" = (true, false)
let%test _ = process_word "abcccd" = (false, true)
let%test _ = process_word "aabcdd" = (true, false)
let%test _ = process_word "abcdee" = (true, false)
let%test _ = process_word "ababab" = (false, true)

let int_of_bool bool =
  match bool with
  | true -> 1
  | false -> 0

let process_words (words: string list) =
  words
  |> List.map process_word
  |> List.fold_left
    (fun (hasExactlyTwoCount, hasExactlyThreeCount) (hasExactlyTwo, hasExactlyThree) -> 
       (
         (hasExactlyTwo |> int_of_bool |> (+) hasExactlyTwoCount), 
         (hasExactlyThree |> int_of_bool |> (+) hasExactlyThreeCount)
       )
    )
    (0, 0)
  |> fun (hasExactlyTwoCount, hasExactlyThreeCount) -> hasExactlyTwoCount * hasExactlyThreeCount
let%test _ = process_words [ "abcdef"; "bababc"; "abbcde"; "abcccd"; "aabcdd"; "abcdee"; "ababab" ] = 12

let part_1 (words: string list): int =
  words
  |> process_words

let (>>) f g x = x |> f |> g
let list_of_string = String.to_seq >> List.of_seq >> List.map (String.make 1)

let compare_two_words a b =
  (list_of_string b) 
  |> List.map2 (fun a b -> 
      if a = b
      then (0, a)
      else (1, "")
    ) (list_of_string a)
  |> List.fold_left
    (fun (countA, charA) (countB, charB) -> 
       (countA + countB, charA ^ charB)
    )
    (0, "")
let%test _ = compare_two_words "abcde" "axcye" = (2, "ace")
let%test _ = compare_two_words "fghij" "fguij" = (1, "fgij")

let compare_each_words (words: string list) =
  Base.Sequence.unfold
    ~init: words
    ~f: (fun state -> 
        match state with
        | current_word :: next_state -> Some (
            List.map (compare_two_words current_word) next_state,
            next_state
          )
        | _ -> None
      )
  |> Base.Sequence.to_list
  |> List.flatten
let%test _ = compare_each_words ["fghij"; "fguij"] = [(1, "fgij")]
let%test _ = compare_each_words ["abcde"; "fghij"; "fguij"] = [(5, ""); (5, ""); (1, "fgij")]

let take_most_closer (diffs: (int * string) list): string = 
  diffs
  |> List.fold_left
    (fun (diffCountA, commonA) (diffCountB, commonB) -> 
       if diffCountA > diffCountB
       then (diffCountB, commonB)
       else (diffCountA, commonA)
    )
    (max_int, "")
  |> snd
let%test _ = take_most_closer [(5, ""); (5, ""); (1, "fgij")] = "fgij"

let part_2 (words: string list) =
  words
  |> compare_each_words
  |> take_most_closer