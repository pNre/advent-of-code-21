open Core

module Point = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp, hash]
  end

  include T
  module Map = Map.Make (T)
end

let read_input file =
  file
  |> In_channel.read_lines
  |> List.map ~f:(fun line ->
         let components = String.split line ~on:' ' in
         let point i =
           let coords =
             List.nth_exn components i
             |> String.split ~on:','
             |> List.map ~f:Int.of_string
           in
           List.nth_exn coords 0, List.nth_exn coords 1
         in
         point 0, point 2)
;;

let is_h_or_v_line ((x1, y1), (x2, y2)) = Int.equal x1 x2 || Int.equal y1 y2

let points_of_line ((x1, y1), (x2, y2)) =
  let min_x = min x1 x2
  and max_x = max x1 x2
  and min_y = min y1 y2
  and max_y = max y1 y2 in
  if Int.equal x1 x2
  then List.init (max_y - min_y + 1) ~f:(fun y -> min_x, min_y + y)
  else if Int.equal y1 y2
  then List.init (max_x - min_x + 1) ~f:(fun x -> min_x + x, min_y)
  else (
    let dist = abs (x1 - x2) + 1 in
    List.init dist ~f:(fun i ->
        (if x1 < x2 then x1 + i else x1 - i), if y1 < y2 then y1 + i else y1 - i))
;;

let count_overlapping lines =
  let points = lines |> List.map ~f:points_of_line |> List.join in
  let _, counter =
    Point.(
      List.fold points ~init:(Map.empty, 0) ~f:(fun (points, counter) point ->
          let points =
            Map.update points point ~f:(fun count -> Option.value count ~default:0 + 1)
          in
          let counter =
            if Int.equal (Map.find_exn points point) 2 then counter + 1 else counter
          in
          points, counter))
  in
  counter
;;

let part1 () =
  let lines = read_input "input" |> List.filter ~f:is_h_or_v_line in
  Printf.printf "1: %d\n" (count_overlapping lines)
;;

let part2 () =
  let lines = read_input "input" in
  Printf.printf "2: %d\n" (count_overlapping lines)
;;

let () =
  part1 ();
  part2 ()
;;
