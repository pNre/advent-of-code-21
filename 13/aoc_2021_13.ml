open Core

let read_manual file =
  let points, folds =
    file
    |> In_channel.read_lines
    |> List.split_while ~f:(fun line -> not (String.is_prefix line ~prefix:"fold"))
  in
  let points =
    points
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:(fun line ->
           let points = line |> String.split ~on:',' |> List.map ~f:Int.of_string in
           List.hd_exn points, List.last_exn points)
  in
  let folds =
    List.map folds ~f:(fun line ->
        let coord = String.chop_prefix_exn ~prefix:"fold along " line in
        let axis = String.get coord 0 in
        match axis with
        | 'x' -> `X (Int.of_string (String.chop_prefix_exn ~prefix:"x=" coord))
        | 'y' -> `Y (Int.of_string (String.chop_prefix_exn ~prefix:"y=" coord))
        | _ -> failwithf "Unexpected instruction: %s" line ())
  in
  points, folds
;;

let offset_of_point ~width (x, y) = (y * width) + x

let size_of_paper points =
  let x = points |> List.map ~f:fst |> List.max_elt ~compare:Int.compare
  and y = points |> List.map ~f:snd |> List.max_elt ~compare:Int.compare in
  Option.value_exn x + 1, Option.value_exn y + 1
;;

let hfold points ~fold_point ~height =
  List.filter_map points ~f:(fun point ->
      match point with
      | x, y when y < fold_point -> Some (x, y)
      | _, y when y = fold_point -> None
      | x, y -> Some (x, height - 1 - y))
;;

let vfold points ~fold_point ~width =
  List.filter_map points ~f:(fun point ->
      match point with
      | x, y when x < fold_point -> Some (x, y)
      | x, _ when x = fold_point -> None
      | x, y -> Some (width - 1 - x, y))
;;

let part1 () =
  let points, folds = read_manual "in.txt" in
  let first_fold = List.hd_exn folds in
  let width, height = size_of_paper points in
  let folded =
    match first_fold with
    | `X x -> vfold ~fold_point:x ~width points
    | `Y y -> hfold ~fold_point:y ~height points
  in
  let width, _ = size_of_paper folded in
  let number_of_points =
    folded |> List.map ~f:(offset_of_point ~width) |> Int.Set.of_list |> Set.length
  in
  Printf.printf "1: %d\n" number_of_points
;;

let part2 () =
  let points, folds = read_manual "in.txt" in
  let width, height = size_of_paper points in
  let folded, width, height =
    List.fold folds ~init:(points, width, height) ~f:(fun (points, width, height) fold ->
        match fold with
        | `X x ->
          let points = vfold ~fold_point:x ~width points in
          points, width / 2, height
        | `Y y ->
          let points = hfold ~fold_point:y ~height points in
          points, width, height / 2)
  in
  let folded_points = folded |> List.map ~f:(offset_of_point ~width) |> Int.Set.of_list in
  Printf.printf "2:\n";
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if Int.Set.mem folded_points (offset_of_point ~width (x, y))
      then Printf.printf "X"
      else Printf.printf " "
    done;
    Printf.printf "\n"
  done
;;

let () =
  part1 ();
  part2 ()
;;
