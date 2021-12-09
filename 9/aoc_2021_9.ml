open Core

module Point = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp, hash]
  end

  include T
  module Set = Set.Make (T)
end

let read_heightmap file =
  file
  |> In_channel.read_lines
  |> Array.of_list_map ~f:(fun row ->
         row |> String.to_array |> Array.map ~f:(Fn.compose Int.of_string Char.to_string))
;;

let neighboorhood_of_point (x, y) width height =
  List.join
    [ (if x > 0 then [ x - 1, y ] else [])
    ; (if x < width - 1 then [ x + 1, y ] else [])
    ; (if y > 0 then [ x, y - 1 ] else [])
    ; (if y < height - 1 then [ x, y + 1 ] else [])
    ]
;;

let low_points heightmap =
  let height = Array.length heightmap in
  Array.foldi heightmap ~init:[] ~f:(fun row_index points row ->
      let width = Array.length row in
      Array.foldi row ~init:points ~f:(fun col_index points h ->
          let n = neighboorhood_of_point (col_index, row_index) width height in
          let n = List.map n ~f:(fun (x, y) -> heightmap.(y).(x)) in
          if List.for_all n ~f:(( < ) h)
          then List.append points [ col_index, row_index ]
          else points))
;;

let part1 () =
  let heightmap = read_heightmap "in.txt" in
  heightmap
  |> low_points
  |> List.map ~f:(fun (x, y) -> heightmap.(y).(x))
  |> List.fold ~init:0 ~f:(fun risk h -> risk + 1 + h)
  |> Printf.printf "1: %d\n"
;;

let find_basin heightmap (x, y) =
  let height = Array.length heightmap in
  let width = Array.length heightmap.(0) in
  let neighbors (x, y) visited =
    let n = neighboorhood_of_point (x, y) width height in
    let h = heightmap.(y).(x) in
    List.filter n ~f:(fun (x, y) ->
        heightmap.(y).(x) < 9
        && heightmap.(y).(x) > h
        && not (Point.Set.mem visited (x, y)))
  in
  let rec visit to_visit visited =
    match to_visit with
    | point :: xs when Point.Set.mem visited point -> visit xs visited
    | point :: xs ->
      let visited = Point.Set.add visited point in
      let adj = neighbors point visited in
      visit (List.append adj xs) visited
    | [] -> Point.Set.length visited
  in
  visit [ x, y ] Point.Set.empty
;;

let part2 () =
  let heightmap = read_heightmap "in.txt" in
  heightmap
  |> low_points
  |> List.map ~f:(fun (x, y) -> find_basin heightmap (x, y))
  |> List.sort ~compare:Int.descending
  |> Fn.flip List.take 3
  |> List.fold ~init:1 ~f:( * )
  |> Printf.printf "2: %d\n"
;;

let () =
  part1 ();
  part2 ()
;;
