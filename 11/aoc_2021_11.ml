open Core

let read_energy_levels file =
  file
  |> In_channel.read_lines
  |> List.map ~f:(fun line ->
         line |> String.to_list |> List.map ~f:(Fn.compose Int.of_string Char.to_string))
;;

let neighborhood_of_offset offset width height =
  let offset_of_point (x, y) width = (y * width) + x
  and point_of_offset offset width = offset % width, offset / width in
  let x, y = point_of_offset offset width in
  [ x - 1, y - 1
  ; x - 1, y
  ; x - 1, y + 1
  ; x, y - 1
  ; x, y + 1
  ; x + 1, y - 1
  ; x + 1, y
  ; x + 1, y + 1
  ]
  |> List.filter ~f:(fun (x, y) -> x >= 0 && y >= 0 && x < width && y < height)
  |> List.map ~f:(fun point -> offset_of_point point width)
;;

let increment_all_levels = List.map ~f:(( + ) 1)

let increment_subset_of_levels neighborhood =
  List.mapi ~f:(fun offset level ->
      if List.mem neighborhood offset ~equal:Int.equal then level + 1 else level)
;;

let find_flashing_offsets =
  List.filter_mapi ~f:(fun offset level -> if level = 10 then Some offset else None)
;;

let solve steps =
  let energy_levels = read_energy_levels "input" in
  let height = List.length energy_levels
  and width = List.length (List.hd_exn energy_levels) in
  let rec count_flashes levels steps step count =
    match steps with
    | Some steps when step = steps -> count
    | None when List.for_all levels ~f:(( = ) 0) -> step
    | _ ->
      let levels = increment_all_levels levels in
      let flashing_offsets = find_flashing_offsets levels in
      let levels = increment_subset_of_levels flashing_offsets levels in
      let rec increment_neighborhood incremented levels count =
        match incremented with
        | [] -> levels, count
        | x :: xs ->
          let neighborhood = neighborhood_of_offset x width height in
          let levels = increment_subset_of_levels neighborhood levels in
          let flashing_offsets = find_flashing_offsets levels in
          let levels = increment_subset_of_levels flashing_offsets levels in
          let incremented = List.append flashing_offsets xs in
          increment_neighborhood incremented levels (count + List.length flashing_offsets)
      in
      let levels, count =
        increment_neighborhood
          flashing_offsets
          levels
          (count + List.length flashing_offsets)
      in
      let reset_levels =
        List.map levels ~f:(fun level -> if level > 9 then 0 else level)
      in
      count_flashes reset_levels steps (step + 1) count
  in
  count_flashes (List.join energy_levels) steps 0 0
;;

let part1 () = Printf.printf "1: %d\n" (solve (Some 100))
let part2 () = Printf.printf "2: %d\n" (solve None)

let () =
  part1 ();
  part2 ()
;;
