open Core

module Node = struct
  module T = struct
    type t = int * int

    let leq (_, a) (_, b) = a <= b
  end

  include T
  module Heap = Containers.Heap.Make (T)
end

let read_map file =
  file
  |> In_channel.read_lines
  |> List.map ~f:(fun line ->
         line |> String.to_list |> List.map ~f:(Fn.compose Int.of_string Char.to_string))
;;

let expand_map map =
  let increment_risk risk factor =
    if risk + factor > 9 then (risk + factor) % 9 else risk + factor
  in
  let increment_risk_of_list list factor =
    List.map list ~f:(fun risk -> increment_risk risk factor)
  in
  let increment_risk_of_map map factor =
    List.map map ~f:(fun row -> List.map row ~f:(fun risk -> increment_risk risk factor))
  in
  let map =
    List.map map ~f:(fun row ->
        let increment_row_risk = increment_risk_of_list row in
        List.concat
          [ increment_row_risk 0
          ; increment_row_risk 1
          ; increment_row_risk 2
          ; increment_row_risk 3
          ; increment_row_risk 4
          ])
  in
  List.concat
    [ increment_risk_of_map map 0
    ; increment_risk_of_map map 1
    ; increment_risk_of_map map 2
    ; increment_risk_of_map map 3
    ; increment_risk_of_map map 4
    ]
;;

let offset_of_point (x, y) width = (y * width) + x
let point_of_offset offset width = offset % width, offset / width

let neighborhood_of_offset offset width height =
  let x, y = point_of_offset offset width in
  [ x - 1, y; x, y - 1; x, y + 1; x + 1, y ]
  |> List.filter ~f:(fun (x, y) -> x >= 0 && y >= 0 && x < width && y < height)
  |> List.map ~f:(fun point -> offset_of_point point width)
;;

let find_best_path map width height =
  let map =
    List.mapi map ~f:(fun offset score -> offset, score) |> Int.Map.of_alist_exn
  in
  let rec visit_neighborhood ~open_set ~paths ~costs ~current neighborhood =
    match neighborhood with
    | [] -> open_set, paths, costs
    | node :: nodes ->
      let risk_cost = Int.Map.find_exn costs current + Int.Map.find_exn map node in
      if risk_cost < Int.Map.find_exn costs node
      then (
        let already_visited = Int.Map.mem paths node in
        let costs = Int.Map.set ~key:node ~data:risk_cost costs in
        let paths = Int.Map.set ~key:node ~data:current paths in
        let open_set =
          if already_visited then open_set else Node.Heap.add open_set (node, risk_cost)
        in
        visit_neighborhood ~open_set ~paths ~costs ~current nodes)
      else visit_neighborhood ~open_set ~paths ~costs ~current nodes
  in
  let rec find_best_path ~open_set ~paths ~costs ~goal =
    match Node.Heap.take open_set with
    | None -> paths
    | Some (_, (current, _)) when current = goal -> paths
    | Some (open_set, (current, _)) ->
      let nodes = neighborhood_of_offset current width height in
      let open_set, paths, costs =
        visit_neighborhood ~open_set ~paths ~costs ~current nodes
      in
      find_best_path ~open_set ~paths ~costs ~goal
  in
  let costs =
    width * height
    |> List.init ~f:(fun offset -> offset, if offset = 0 then 0 else Int.max_value)
    |> Int.Map.of_alist_exn
  in
  let goal = (width * height) - 1 in
  let paths =
    find_best_path
      ~open_set:(Node.Heap.of_list [ 0, 0 ])
      ~paths:Int.Map.empty
      ~costs
      ~goal
  in
  let rec find_path node paths path =
    match Int.Map.find paths node with
    | None -> List.tl_exn path
    | Some previous -> find_path previous paths (List.append [ previous ] path)
  in
  let path = find_path goal paths [ goal ] in
  List.sum (module Int) path ~f:(Int.Map.find_exn map)
;;

let part1 () =
  let map = read_map "input" in
  let width = List.length (List.hd_exn map) in
  let height = List.length map in
  Printf.printf "1: %d\n" (find_best_path (List.concat map) width height)
;;

let part2 () =
  let map = read_map "input" |> expand_map in
  let width = List.length (List.hd_exn map) in
  let height = List.length map in
  Printf.printf "2: %d\n" (find_best_path (List.concat map) width height)
;;

let () =
  part1 ();
  part2 ()
;;
