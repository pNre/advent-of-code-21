open Core

let read_target_area file =
  let input = In_channel.read_all file in
  let target_area = String.chop_prefix_exn input ~prefix:"target area: " in
  let ranges = target_area |> String.split ~on:',' |> List.map ~f:String.strip in
  let range s prefix =
    s
    |> String.chop_prefix_exn ~prefix
    |> String.split ~on:'.'
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:Int.of_string
  in
  let x_range = range (List.hd_exn ranges) "x="
  and y_range = range (List.last_exn ranges) "y=" in
  ( (List.hd_exn x_range, List.last_exn x_range)
  , (List.hd_exn y_range, List.last_exn y_range) )
;;

let is_point_in_rect (x, y) ((min_x, max_x), (min_y, max_y)) =
  x >= min_x && x <= max_x && y >= min_y && y <= max_y
;;

let is_point_past_rect (x, y) ((_, max_x), (min_y, _)) = x > max_x || y < min_y

let max_height (vx, vy) rect =
  let rec find_height (x, y) (vx, vy) rect max_h =
    if is_point_past_rect (x, y) rect
    then None
    else if is_point_in_rect (x, y) rect
    then Some max_h
    else (
      let x = x + vx
      and y = y + vy in
      let vy = vy - 1 in
      let vx = Int.max 0 (vx - 1) in
      find_height (x, y) (vx, vy) rect (Int.max max_h y))
  in
  find_height (0, 0) (vx, vy) rect 0
;;

let part1 () =
  let (min_x, max_x), (min_y, max_y) = read_target_area "input" in
  let vxs = List.init max_x ~f:(( + ) 1) |> List.sort ~compare:Int.compare in
  let vy_range = Int.max (Int.abs min_y) (Int.abs max_y) in
  let vys = List.init ((vy_range * 2) + 1) ~f:(fun i -> vy_range - i) in
  let vs = List.cartesian_product vxs vys in
  let heights =
    vs
    |> List.map ~f:(fun v -> max_height v ((min_x, max_x), (min_y, max_y)))
    |> List.filter_opt
    |> List.sort ~compare:Int.ascending
  in
  Printf.printf "1: %d\n" (List.last_exn heights)
;;

let part2 () =
  let (min_x, max_x), (min_y, max_y) = read_target_area "input" in
  let vxs = List.init max_x ~f:(( + ) 1) |> List.sort ~compare:Int.compare in
  let vy_range = Int.max (Int.abs min_y) (Int.abs max_y) in
  let vys = List.init ((vy_range * 2) + 1) ~f:(fun i -> vy_range - i) in
  let vs = List.cartesian_product vxs vys in
  let heights =
    vs
    |> List.map ~f:(fun v -> max_height v ((min_x, max_x), (min_y, max_y)))
    |> List.filter_opt
    |> List.sort ~compare:Int.ascending
  in
  Printf.printf "2: %d\n" (List.length heights)
;;

let () =
  part1 ();
  part2 ()
;;
