open Util
open Core_kernel

type node = string
type edge = node * node

module Node_set = Set.Make(String)

let edge_regexp = Re.Pcre.regexp "^Step (.) must be finished before step (.) can begin.$"
let edge_of_string (s:string): edge =
  let matches = s |> Re.exec edge_regexp in
  (
    Re.get matches 1,
    Re.get matches 2
  )

let%test _ =  "Step C must be finished before step A can begin." |> edge_of_string |> (=) ("C", "A")
let%test _ =  "Step C must be finished before step F can begin." |> edge_of_string |> (=) ("C", "F")
let%test _ =  "Step A must be finished before step B can begin." |> edge_of_string |> (=) ("A", "B")
let%test _ =  "Step A must be finished before step D can begin." |> edge_of_string |> (=) ("A", "D")
let%test _ =  "Step B must be finished before step E can begin." |> edge_of_string |> (=) ("B", "E")
let%test _ =  "Step D must be finished before step E can begin." |> edge_of_string |> (=) ("D", "E")
let%test _ =  "Step F must be finished before step E can begin." |> edge_of_string |> (=) ("F", "E")

let get_parents (edges: edge list) (node: node): Node_set.t =
  edges
  |> List.filter ~f:(snd >> (=) node)
  |> List.map ~f:fst
  |> Node_set.of_list
let%test _ = 
  "E"
  |> get_parents [
    ("C", "A");
    ("C", "F");
    ("A", "B");
    ("A", "D");
    ("B", "E");
    ("D", "E");
    ("F", "E");
  ]
  |> Node_set.equal (["B"; "D"; "F"] |> Node_set.of_list)

let get_children (edges: edge list) (node: node): Node_set.t =
  edges
  |> List.filter ~f:(fst >> (=) node)
  |> List.map ~f:snd
  |> Node_set.of_list
let%test _ = 
  "A"
  |> get_children [
    ("C", "A");
    ("C", "F");
    ("A", "B");
    ("A", "D");
    ("B", "E");
    ("D", "E");
    ("F", "E");
  ]
  |> Node_set.equal (["B"; "D"] |> Node_set.of_list)

let get_roots (edges: edge list): Node_set.t =
  let froms = edges |> List.map ~f:fst |> Node_set.of_list in
  let tos = edges |> List.map ~f:snd |> Node_set.of_list in

  Node_set.diff froms tos


let%test _ =
  [
    ("C", "A");
    ("C", "F");
    ("A", "B");
    ("A", "D");
    ("B", "E");
    ("D", "E");
    ("F", "E");
  ]
  |> get_roots
  |> Node_set.equal (["C"] |> Node_set.of_list)

let run (edges: edge list): node list =
  let roots = get_roots edges in
  let rec loop (edges: edge list) (completed_nodes: node list) (available_nodes: node list): node list =
    match available_nodes with
    | [] -> completed_nodes
    | hd :: tail ->
      let parents = get_parents edges hd in
      match Node_set.is_subset parents ~of_:(Node_set.of_list completed_nodes) with
      | true -> 
        let children = hd |> get_children edges |> Node_set.to_list in
        loop edges
          (completed_nodes @ [hd])
          (children @ tail |> List.dedup_and_sort ~compare:String.compare)
      | false -> loop edges completed_nodes tail in

  roots
  |> Node_set.to_list
  |> loop edges [] 
let%test _ = 
  [
    ("C", "A");
    ("C", "F");
    ("A", "B");
    ("A", "D");
    ("B", "E");
    ("D", "E");
    ("F", "E");
  ]
  |> run
  |> (=) ["C"; "A"; "B"; "D"; "F"; "E"]

let part_1 (input: string list): string =
  input
  |> List.map ~f:edge_of_string
  |> run
  |> String.concat ~sep:""
let%test _ =
  [
    "Step C must be finished before step A can begin.";
    "Step C must be finished before step F can begin.";
    "Step A must be finished before step B can begin.";
    "Step A must be finished before step D can begin.";
    "Step B must be finished before step E can begin.";
    "Step D must be finished before step E can begin.";
    "Step F must be finished before step E can begin.";
  ]
  |> part_1
  |> (=) "CABDFE"
let%test _ =
  [
    "Step A must be finished before step B can begin.";
    "Step A must be finished before step D can begin.";
    "Step A must be finished before step F can begin.";
    "Step B must be finished before step C can begin.";
    "Step B must be finished before step D can begin.";
    "Step D must be finished before step C can begin.";
  ]
  |> part_1
  |> (=) "ABDCF"

let get_duration (node: node): int =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  |> String.to_list
  |> List.map ~f:Char.to_string
  |> List.findi ~f:(fun _ -> (=) node)
  |> Option.map ~f:fst
  |> Option.value ~default:0
  |> (+) 1
let%test _ = "A" |> get_duration |> (=) 1
let%test _ = "Z" |> get_duration |> (=) 26

let run2 (worker_count: int) (min_duration: int) (edges: edge list): int =
  let roots = get_roots edges in
  let rec loop (workers: (int * node) list) (edges: edge list) (completed_nodes: node list) (available_nodes: node list): int =
    match (workers |> List.length, available_nodes) with
    | (size, []) when size = 0 -> 0
    | (size, hd :: tail) when size < worker_count ->
      let parents = get_parents edges hd in
      ( match Node_set.is_subset parents ~of_:(Node_set.of_list completed_nodes) with
        | true -> 
          (* let children = hd |> get_children edges |> Node_set.to_list in *)
          loop
            (workers @ [(get_duration hd |> (+) min_duration, hd)])
            edges
            (completed_nodes)
            (tail |> List.dedup_and_sort ~compare:String.compare)
        | false -> loop workers edges completed_nodes tail)
    | _ ->
      let (finished_workers, pending_workers) = workers |> List.partition_tf ~f:(fst >> (=) 1) in
      loop 
        (pending_workers |> List.map ~f:(Tuple2.map_fst ~f:(fun a -> a - 1)))
        edges
        (completed_nodes @ (finished_workers |> List.map ~f:(snd)))
        (available_nodes @ (finished_workers |> List.map ~f:(snd >> get_children edges >> Node_set.to_list) |> List.concat))
      |> (+) 1 in


  roots
  |> Node_set.to_list
  |> loop [] edges []
let%test _ = 
  [
    ("C", "A");
    ("C", "F");
    ("A", "B");
    ("A", "D");
    ("B", "E");
    ("D", "E");
    ("F", "E");
  ]
  |> run2 2 0
  |> (=) 15

let part_2 (worker_count: int) (min_duration: int) (input: string list): int =
  input
  |> List.map ~f:edge_of_string
  |> run2 worker_count min_duration
let%test _ =
  [
    "Step C must be finished before step A can begin.";
    "Step C must be finished before step F can begin.";
    "Step A must be finished before step B can begin.";
    "Step A must be finished before step D can begin.";
    "Step B must be finished before step E can begin.";
    "Step D must be finished before step E can begin.";
    "Step F must be finished before step E can begin.";
  ]
  |> part_2 2 0
  |> (=) 15