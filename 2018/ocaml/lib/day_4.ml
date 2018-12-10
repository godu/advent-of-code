open Core_kernel

type state =
  | WakeUp
  | FallAsleep
  | BeginShift of int

let string_of_state = function
  | WakeUp -> "wakes up"
  | FallAsleep -> "falls asleep"
  | BeginShift id -> sprintf "Guard #%d begins shift" id

type record = Time.t * state

let string_of_record (time, state) =
  sprintf "[%s] %s" (Time.to_string time) (string_of_state state)


let record_regexp = Re.Pcre.regexp "^\\[(.*)\\] (.*)$"
let extract_time_state s =
  let matches = s 
                |> Re.exec record_regexp 
                |> Re.get_all in
  (
    Array.get matches 1, 
    Array.get matches 2
  )
let%test _ = "[1518-11-01 00:00] Guard #10 begins shift" |> extract_time_state = ("1518-11-01 00:00", "Guard #10 begins shift")

let begin_shift_regexp = Re.Pcre.regexp "^Guard #(\\d+) begins shift$"
let state_of_string s =
  match s with
  | "falls asleep" -> FallAsleep
  | "wakes up" -> WakeUp
  | s -> 
    s 
    |> Re.exec begin_shift_regexp 
    |> (fun subs -> Re.get subs 1)
    |> (fun i -> BeginShift (int_of_string i))
let%test _ = "falls asleep" |> state_of_string = FallAsleep
let%test _ = "wakes up" |> state_of_string = WakeUp
let%test _ = "Guard #99 begins shift" |> state_of_string = BeginShift 99

let record_of_string (recordString: string): record =
  let (timeString, stateString) = recordString |> extract_time_state in
  ( 
    timeString |> Time.of_string,
    stateString |> state_of_string
  )
let%test _ = "[1518-11-01 00:00] Guard #10 begins shift" |> record_of_string = (Time.of_string "1518-11-01 00:00", BeginShift 10)
let%test _ = "[1518-11-01 00:05] falls asleep" |> record_of_string = (Time.of_string "1518-11-01 00:05", FallAsleep)
let%test _ = "[1518-11-01 00:25] wakes up" |> record_of_string = (Time.of_string "1518-11-01 00:25", WakeUp)
let%test _ = "[1518-11-01 00:30] falls asleep" |> record_of_string = (Time.of_string "1518-11-01 00:30", FallAsleep)
let%test _ = "[1518-11-01 00:55] wakes up" |> record_of_string = (Time.of_string "1518-11-01 00:55", WakeUp)
let%test _ = "[1518-11-01 23:58] Guard #99 begins shift" |> record_of_string = (Time.of_string "1518-11-01 23:58", BeginShift 99)
let%test _ = "[1518-11-02 00:40] falls asleep" |> record_of_string = (Time.of_string "1518-11-02 00:40", FallAsleep)
let%test _ = "[1518-11-02 00:50] wakes up" |> record_of_string = (Time.of_string "1518-11-02 00:50", WakeUp)
let%test _ = "[1518-11-03 00:05] Guard #10 begins shift" |> record_of_string = (Time.of_string "1518-11-03 00:05", BeginShift 10)
let%test _ = "[1518-11-03 00:24] falls asleep" |> record_of_string = (Time.of_string "1518-11-03 00:24", FallAsleep)
let%test _ = "[1518-11-03 00:29] wakes up" |> record_of_string = (Time.of_string "1518-11-03 00:29", WakeUp)
let%test _ = "[1518-11-04 00:02] Guard #99 begins shift" |> record_of_string = (Time.of_string "1518-11-04 00:02", BeginShift 99)
let%test _ = "[1518-11-04 00:36] falls asleep" |> record_of_string = (Time.of_string "1518-11-04 00:36", FallAsleep)
let%test _ = "[1518-11-04 00:46] wakes up" |> record_of_string = (Time.of_string "1518-11-04 00:46", WakeUp)
let%test _ = "[1518-11-05 00:03] Guard #99 begins shift" |> record_of_string = (Time.of_string "1518-11-05 00:03", BeginShift 99)
let%test _ = "[1518-11-05 00:45] falls asleep" |> record_of_string = (Time.of_string "1518-11-05 00:45", FallAsleep)
let%test _ = "[1518-11-05 00:55] wakes up" |> record_of_string = (Time.of_string "1518-11-05 00:55", WakeUp)

let compare_records a b = Time.compare (fst a) (fst b)
let%test _ = 
  compare_records (Time.of_string "1518-11-01 00:25", WakeUp) (Time.of_string "1518-10-01 00:25", WakeUp)
  |> (=) 1
let group_by_shift (records: record list): (int * record list) list =
  records
  |> List.group ~break:(fun _ b ->
      match snd b with
      | BeginShift _ -> true
      | _ -> false
    )
  |> List.map ~f:(fun records -> 
      match records with
      | (_, BeginShift id) :: records -> (id, records)
      | _ -> (0, [])
    )
let%test _ = 
  [
    (Time.of_string "1518-11-01 00:00", BeginShift 10);
    (Time.of_string "1518-11-01 00:05", FallAsleep);
    (Time.of_string "1518-11-01 00:25", WakeUp);
    (Time.of_string "1518-11-01 00:30", FallAsleep);
    (Time.of_string "1518-11-01 00:55", WakeUp);
    (Time.of_string "1518-11-01 23:58", BeginShift 99);
    (Time.of_string "1518-11-02 00:40", FallAsleep);
    (Time.of_string "1518-11-02 00:50", WakeUp);
    (Time.of_string "1518-11-03 00:05", BeginShift 10);
    (Time.of_string "1518-11-03 00:24", FallAsleep);
    (Time.of_string "1518-11-03 00:29", WakeUp);
    (Time.of_string "1518-11-04 00:02", BeginShift 99);
    (Time.of_string "1518-11-04 00:36", FallAsleep);
    (Time.of_string "1518-11-04 00:46", WakeUp);
    (Time.of_string "1518-11-05 00:03", BeginShift 99);
    (Time.of_string "1518-11-05 00:45", FallAsleep);
    (Time.of_string "1518-11-05 00:55", WakeUp);
  ]
  |> group_by_shift
  |> (=) [
    (
      10,
      [
        (Time.of_string "1518-11-01 00:05", FallAsleep);
        (Time.of_string "1518-11-01 00:25", WakeUp);
        (Time.of_string "1518-11-01 00:30", FallAsleep);
        (Time.of_string "1518-11-01 00:55", WakeUp);
      ]
    );
    (
      99,
      [
        (Time.of_string "1518-11-02 00:40", FallAsleep);
        (Time.of_string "1518-11-02 00:50", WakeUp);
      ]
    );
    (
      10,
      [
        (Time.of_string "1518-11-03 00:24", FallAsleep);
        (Time.of_string "1518-11-03 00:29", WakeUp);
      ]
    );
    (
      99,
      [
        (Time.of_string "1518-11-04 00:36", FallAsleep);
        (Time.of_string "1518-11-04 00:46", WakeUp);
      ]
    );
    (
      99,
      [
        (Time.of_string "1518-11-05 00:45", FallAsleep);
        (Time.of_string "1518-11-05 00:55", WakeUp);
      ]
    );
  ]

let group_by_guard (records: (int * record list) list): (int * record list) list =
  records
  |> List.sort ~compare: (fun a b -> Int.compare (fst a) (fst b))
  |> List.group ~break: (fun a b -> (fst a) <> (fst b))
  |> List.map ~f:(fun l ->
      let id = l |> List.hd_exn |> fst in
      let records = 
        l
        |> List.map ~f:snd
        |> List.concat in
      (id, records)
    )
let%test _ = 
  [
    (
      10,
      [
        (Time.of_string "1518-11-01 00:05", FallAsleep);
        (Time.of_string "1518-11-01 00:25", WakeUp);
        (Time.of_string "1518-11-01 00:30", FallAsleep);
        (Time.of_string "1518-11-01 00:55", WakeUp);
      ]
    );
    (
      99,
      [
        (Time.of_string "1518-11-02 00:40", FallAsleep);
        (Time.of_string "1518-11-02 00:50", WakeUp);
      ]
    );
    (
      10,
      [
        (Time.of_string "1518-11-03 00:24", FallAsleep);
        (Time.of_string "1518-11-03 00:29", WakeUp);
      ]
    );
    (
      99,
      [
        (Time.of_string "1518-11-04 00:36", FallAsleep);
        (Time.of_string "1518-11-04 00:46", WakeUp);
      ]
    );
    (
      99,
      [
        (Time.of_string "1518-11-05 00:45", FallAsleep);
        (Time.of_string "1518-11-05 00:55", WakeUp);
      ]
    );
  ]
  |> group_by_guard
  |> (=) [
    (
      10,
      [
        (Time.of_string "1518-11-01 00:05", FallAsleep);
        (Time.of_string "1518-11-01 00:25", WakeUp);
        (Time.of_string "1518-11-01 00:30", FallAsleep);
        (Time.of_string "1518-11-01 00:55", WakeUp);
        (Time.of_string "1518-11-03 00:24", FallAsleep);
        (Time.of_string "1518-11-03 00:29", WakeUp);
      ]
    );
    (
      99,
      [
        (Time.of_string "1518-11-02 00:40", FallAsleep);
        (Time.of_string "1518-11-02 00:50", WakeUp);
        (Time.of_string "1518-11-04 00:36", FallAsleep);
        (Time.of_string "1518-11-04 00:46", WakeUp);
        (Time.of_string "1518-11-05 00:45", FallAsleep);
        (Time.of_string "1518-11-05 00:55", WakeUp);
      ]
    );
  ]

let group_by_sleep (records: record list): record list list =
  records
  |> List.group ~break:(fun _ b ->
      match snd b with
      | FallAsleep -> true
      | _ -> false
    )
let%test _ =

  [
    (Time.of_string "1518-11-02 00:40", FallAsleep);
    (Time.of_string "1518-11-02 00:50", WakeUp);
    (Time.of_string "1518-11-04 00:36", FallAsleep);
    (Time.of_string "1518-11-04 00:46", WakeUp);
    (Time.of_string "1518-11-05 00:45", FallAsleep);
    (Time.of_string "1518-11-05 00:55", WakeUp);
  ]
  |> group_by_sleep
  |> (=) 
    [
      [
        (Time.of_string "1518-11-02 00:40", FallAsleep);
        (Time.of_string "1518-11-02 00:50", WakeUp);
      ];
      [
        (Time.of_string "1518-11-04 00:36", FallAsleep);
        (Time.of_string "1518-11-04 00:46", WakeUp);
      ];
      [
        (Time.of_string "1518-11-05 00:45", FallAsleep);
        (Time.of_string "1518-11-05 00:55", WakeUp);
      ];
    ]


let rec range_time start_time end_time =
  match Time.compare start_time end_time with
  | 0 -> []
  | _ -> 
    start_time :: (range_time (Time.add start_time Time.Span.minute) end_time) 

let minutes_asleep_of_records (records: record list): Time.t list = 
  match records with
  | [(asleep_time, FallAsleep); (awake_time, WakeUp)] -> range_time asleep_time awake_time
  | _ -> []
let%test _ = 
  [
    (Time.of_string "1518-11-05 00:45", FallAsleep);
    (Time.of_string "1518-11-05 00:55", WakeUp);
  ]
  |> minutes_asleep_of_records
  |> (=) [
    Time.of_string "1518-11-05 00:45";
    Time.of_string "1518-11-05 00:46";
    Time.of_string "1518-11-05 00:47";
    Time.of_string "1518-11-05 00:48";
    Time.of_string "1518-11-05 00:49";
    Time.of_string "1518-11-05 00:50";
    Time.of_string "1518-11-05 00:51";
    Time.of_string "1518-11-05 00:52";
    Time.of_string "1518-11-05 00:53";
    Time.of_string "1518-11-05 00:54";
  ]

let time_regexp = Re.Pcre.regexp "^(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)"
let minute_of_time t =
  let matches =
    t
    |> Time.to_string
    |> Re.exec time_regexp 
    |> Re.get_all in
  Array.get matches 5
  |> int_of_string 

let part_1 (record_strings: string list): int =
  record_strings
  |> List.map ~f:record_of_string
  |> List.sort ~compare:compare_records
  |> group_by_shift
  |> group_by_guard
  |> List.map ~f:(fun (id, records) -> 
      (
        id,
        records
        |> group_by_sleep
        |> List.map ~f:(minutes_asleep_of_records)
        |> List.concat
      )
    )
  |> List.max_elt ~compare:(fun a b -> Int.compare (a |> snd |> List.length) (b |> snd |> List.length))
  |> Base.Option.value ~default:(0, [])
  |> (fun (id, minutes_asleep) -> 
      let most_asleep_minute = 
        minutes_asleep
        |> List.map ~f: minute_of_time
        |> List.sort ~compare:Int.compare
        |> List.group ~break:(<>)
        |> List.sort ~compare:(fun a b -> Int.compare (List.length b) (List.length a))
        |> List.hd_exn
        |> List.hd_exn in
      id * most_asleep_minute
    )

let%test _ =
  [  
    "[1518-11-01 00:00] Guard #10 begins shift";
    "[1518-11-01 00:05] falls asleep";
    "[1518-11-01 00:25] wakes up";
    "[1518-11-01 00:30] falls asleep";
    "[1518-11-01 00:55] wakes up";
    "[1518-11-01 23:58] Guard #99 begins shift";
    "[1518-11-02 00:40] falls asleep";
    "[1518-11-02 00:50] wakes up";
    "[1518-11-03 00:05] Guard #10 begins shift";
    "[1518-11-03 00:24] falls asleep";
    "[1518-11-03 00:29] wakes up";
    "[1518-11-04 00:02] Guard #99 begins shift";
    "[1518-11-04 00:36] falls asleep";
    "[1518-11-04 00:46] wakes up";
    "[1518-11-05 00:03] Guard #99 begins shift";
    "[1518-11-05 00:45] falls asleep";
    "[1518-11-05 00:55] wakes up";
  ]
  |> part_1
  |> (=) 240

let find_most_asleep_minute (minutes_asleep: int list) =
  minutes_asleep
  |> List.sort ~compare:Int.compare
  |> List.group ~break:(<>)
  |> List.max_elt ~compare:(fun a b -> Int.compare (a |> List.length) (b |> List.length))
  |> Base.Option.map ~f:(fun a -> (a |> List.hd_exn, a |> List.length))
  |> Base.Option.value ~default:(0, 0)

let%test _ =
  [5; 6; 7; 3; 4; 5; 4; 5; 6;]
  |> find_most_asleep_minute
  |> (=) (5, 3)

let part_2 (record_strings: string list): int =
  record_strings
  |> List.map ~f:record_of_string
  |> List.sort ~compare:compare_records
  |> group_by_shift
  |> group_by_guard
  |> List.map ~f:(fun (id, records) -> 
      (
        id,
        records
        |> group_by_sleep
        |> List.map ~f:(minutes_asleep_of_records)
        |> List.concat
        |> List.map ~f:minute_of_time
      )
    )
  |> List.map ~f:(fun (id, minutes_asleep) -> 
      let (minute, count) = minutes_asleep |> find_most_asleep_minute in
      (
        id,
        minute,
        count
      )
    )
  |> List.max_elt ~compare:(fun a b -> Int.compare (a |> Tuple3.get3) (b |> Tuple3.get3))
  |> Base.Option.value_map ~default:0 ~f:(fun (id, minute, _) -> id * minute)
let%test _ =
  [  
    "[1518-11-01 00:00] Guard #10 begins shift";
    "[1518-11-01 00:05] falls asleep";
    "[1518-11-01 00:25] wakes up";
    "[1518-11-01 00:30] falls asleep";
    "[1518-11-01 00:55] wakes up";
    "[1518-11-01 23:58] Guard #99 begins shift";
    "[1518-11-02 00:40] falls asleep";
    "[1518-11-02 00:50] wakes up";
    "[1518-11-03 00:05] Guard #10 begins shift";
    "[1518-11-03 00:24] falls asleep";
    "[1518-11-03 00:29] wakes up";
    "[1518-11-04 00:02] Guard #99 begins shift";
    "[1518-11-04 00:36] falls asleep";
    "[1518-11-04 00:46] wakes up";
    "[1518-11-05 00:03] Guard #99 begins shift";
    "[1518-11-05 00:45] falls asleep";
    "[1518-11-05 00:55] wakes up";
  ]
  |> part_2
  |> (=) 4455 