open Core

let read_digits file =
  let patterns, outputs =
    file
    |> In_channel.read_lines
    |> List.map ~f:(fun line ->
           let components = String.split ~on:'|' line in
           let pattern = components |> List.hd_exn |> String.strip |> String.split ~on:' '
           and output =
             components |> List.last_exn |> String.strip |> String.split ~on:' '
           in
           pattern, output)
    |> List.unzip
  in
  patterns, outputs
;;

let part1 () =
  let _, outputs = read_digits "input" in
  let easy_count =
    outputs
    |> List.join
    |> List.fold ~init:0 ~f:(fun count output ->
           let len = String.length output in
           count + Bool.to_int (len = 2 || len = 4 || len = 3 || len = 7))
  in
  Printf.printf "1: %d\n" easy_count
;;

let char_set_of_string = Fn.compose Char.Set.of_list String.to_list

let map_patterns_to_digits patterns =
  let is_subset pattern1 pattern2 =
    let a = char_set_of_string pattern1
    and b = char_set_of_string pattern2 in
    Char.Set.is_subset b ~of_:a
  in
  let are_disjoint pattern_set1 pattern2 =
    let b = char_set_of_string pattern2 in
    Char.Set.are_disjoint pattern_set1 b
  in
  let diff pattern1 pattern2 =
    let a = char_set_of_string pattern1
    and b = char_set_of_string pattern2 in
    Char.Set.diff a b
  in
  let open Int in
  let map =
    List.fold patterns ~init:Map.empty ~f:(fun map x ->
        match x with
        | x when String.length x = 2 -> Map.set map ~key:1 ~data:x
        | x when String.length x = 4 -> Map.set map ~key:4 ~data:x
        | x when String.length x = 3 -> Map.set map ~key:7 ~data:x
        | x when String.length x = 7 -> Map.set map ~key:8 ~data:x
        | x when String.length x = 5 ->
          (* 2, 3, 5 *)
          map
        | x when String.length x = 6 ->
          (* 0, 6, 9 *)
          map
        | x -> failwithf "Unhandled: %s\n" x ())
  in
  let map =
    List.fold patterns ~init:map ~f:(fun map x ->
        match x with
        | x when String.length x = 5 && is_subset x (Map.find_exn map 1) ->
          Map.set map ~key:3 ~data:x
        | x when String.length x = 5 ->
          (* 2, 5 *)
          map
        | x when String.length x = 6 ->
          if is_subset x (Map.find_exn map 4)
          then Map.set map ~key:9 ~data:x
          else if is_subset x (Map.find_exn map 7)
          then Map.set map ~key:0 ~data:x
          else Map.set map ~key:6 ~data:x
        | _ -> map)
  in
  let map =
    List.fold patterns ~init:map ~f:(fun map x ->
        match x with
        | x when String.length x = 5 ->
          if is_subset x (Map.find_exn map 1)
          then map
          else if are_disjoint (diff (Map.find_exn map 6) x) (Map.find_exn map 1)
          then Map.set map ~key:5 ~data:x
          else Map.set map ~key:2 ~data:x
        | _ -> map)
  in
  map
  |> Map.to_alist ~key_order:`Increasing
  |> List.map ~f:snd
  |> List.map ~f:char_set_of_string
;;

let part2 () =
  let patterns, outputs = read_digits "input" in
  let patterns_and_outputs = List.zip_exn patterns outputs in
  let result =
    List.fold patterns_and_outputs ~init:0 ~f:(fun sum (patterns, outputs) ->
        let digits = map_patterns_to_digits patterns in
        sum
        + List.fold outputs ~init:0 ~f:(fun num output ->
              let segments_set = char_set_of_string output in
              let digit =
                List.find_mapi digits ~f:(fun idx digit_segments_set ->
                    if Char.Set.equal segments_set digit_segments_set
                    then Some idx
                    else None)
              in
              (num * 10) + Option.value_exn digit))
  in
  Printf.printf "2: %d\n" result
;;

let () =
  part1 ();
  part2 ()
;;
