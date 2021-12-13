open Core

let read_paths file =
  file
  |> In_channel.read_lines
  |> List.map ~f:(fun line ->
         let ends = String.split ~on:'-' line in
         List.nth_exn ends 0, List.nth_exn ends 1)
  |> List.fold ~init:String.Map.empty ~f:(fun map (a, b) ->
         let append_path a b map =
           Map.update map a ~f:(function
               | Some nodes -> String.Set.add nodes b
               | None -> String.Set.of_list [ b ])
         in
         if String.equal a "start" || String.equal b "end"
         then append_path a b map
         else if String.equal b "start" || String.equal a "end"
         then append_path b a map
         else map |> append_path a b |> append_path b a)
;;

let count_distinct_paths caves can_visit_small_caves_twice =
  let is_small_cave = String.for_all ~f:Char.is_lowercase in
  let can_visit_node visited x =
    match String.Map.find visited x with
    | None -> true
    | Some 1 when can_visit_small_caves_twice ->
      let has_visited_twice = String.Map.exists visited ~f:(( = ) 2) in
      not has_visited_twice
    | Some _ -> false
  in
  let rec visit caves visited node =
    match node with
    | "end" -> 1
    | x ->
      let edges = String.Map.find_exn caves x in
      let visited =
        if is_small_cave x
        then
          String.Map.update visited x ~f:(fun visits ->
              Option.value ~default:0 visits + 1)
        else visited
      in
      edges
      |> Set.filter ~f:(can_visit_node visited)
      |> Set.sum (module Int) ~f:(visit caves visited)
  in
  visit caves String.Map.empty "start"
;;

let part1 () =
  let caves = read_paths "input" in
  Printf.printf "1: %d\n" (count_distinct_paths caves false)
;;

let part2 () =
  let caves = read_paths "input" in
  Printf.printf "2: %d\n" (count_distinct_paths caves true)
;;

let () =
  part1 ();
  part2 ()
;;
