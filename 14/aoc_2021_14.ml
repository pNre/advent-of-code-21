open Core

let read_polymer file =
  let lines = In_channel.read_lines file in
  let template = List.hd_exn lines in
  let insertion_rules =
    List.drop lines 2
    |> List.fold ~init:String.Map.empty ~f:(fun rules rule ->
           let pair = String.prefix rule 2
           and insertion = String.suffix rule 1 in
           String.Map.set ~key:pair ~data:insertion rules)
  in
  template, insertion_rules
;;

let insert template insertion_rules steps =
  let rec insert' polymer insertion_rules steps =
    let rec replace replaced source pos =
      if pos = String.length source - 1
      then replaced ^ String.suffix source 1
      else (
        let pair = String.sub source ~pos ~len:2 in
        match String.Map.find insertion_rules pair with
        | Some replacement ->
          replace (replaced ^ String.prefix pair 1 ^ replacement) source (pos + 1)
        | None -> replace (replaced ^ String.prefix pair 1) source (pos + 1))
    in
    if steps = 0
    then polymer
    else insert' (replace "" polymer 0) insertion_rules (steps - 1)
  in
  insert' template insertion_rules steps
;;

let count_element_frequency s =
  let frequencies =
    s
    |> String.fold ~init:Char.Map.empty ~f:(fun counter ch ->
           Char.Map.update counter ch ~f:(function
               | None -> 1
               | Some x -> x + 1))
    |> Char.Map.to_alist
    |> List.map ~f:snd
  in
  let max = List.max_elt frequencies ~compare:Int.compare |> Option.value_exn in
  let min = List.min_elt frequencies ~compare:Int.compare |> Option.value_exn in
  max - min
;;

let part1 () =
  let template, insertion_rules = read_polymer "input" in
  let result = insert template insertion_rules 10 in
  Printf.printf "1: %d\n" (count_element_frequency result)
;;

let count_frequency template insertion_rules steps =
  let set_or_sum map key data =
    String.Map.update map key ~f:(Option.value_map ~default:data ~f:(( + ) data))
  in
  let apply_insertion_rules pair =
    let insertion = String.Map.find_exn insertion_rules pair in
    String.prefix pair 1 ^ insertion, insertion ^ String.suffix pair 1
  in
  let rec count_frequency pairs_count ~steps =
    if steps = 0
    then (
      let frequencies =
        pairs_count
        |> String.Map.fold ~init:String.Map.empty ~f:(fun ~key ~data map ->
               set_or_sum map (String.prefix key 1) data)
        |> String.Map.data
      in
      let max = List.max_elt frequencies ~compare:Int.compare |> Option.value_exn in
      let min = List.min_elt frequencies ~compare:Int.compare |> Option.value_exn in
      max - min)
    else
      pairs_count
      |> String.Map.fold ~init:String.Map.empty ~f:(fun ~key ~data pairs_count ->
             if String.length key > 1
             then (
               let l, r = apply_insertion_rules key in
               set_or_sum (set_or_sum pairs_count l data) r data)
             else set_or_sum pairs_count key data)
      |> count_frequency ~steps:(steps - 1)
  in
  String.length template - 1
  |> List.init ~f:(fun pos -> String.sub template ~pos ~len:2, 1)
  |> List.append [ String.suffix template 1, 1 ]
  |> String.Map.of_alist_reduce ~f:( + )
  |> count_frequency ~steps
;;

let part2 () =
  let template, insertion_rules = read_polymer "input" in
  Printf.printf "2: %d\n" (count_frequency template insertion_rules 40)
;;

let () =
  part1 ();
  part2 ()
;;
