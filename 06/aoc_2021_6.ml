open Core

let read_ages file =
  file |> In_channel.read_all |> String.split ~on:',' |> List.map ~f:Int.of_string
;;

let sim ages days =
  let cache = Array.make_matrix ~dimx:8 ~dimy:(days + 1) 0 in
  let rec spawn_count age days_left =
    if days_left > age
    then (
      let days_left = days_left - (age + 1) in
      match cache.(age).(days_left) with
      | 0 ->
        let spawned = 1 + (days_left / 7) in
        let partial =
          spawned
          |> List.init ~f:(fun i -> spawn_count 0 (days_left - (7 * i) - 8))
          |> List.fold ~init:0 ~f:( + )
        in
        let result = spawned + partial in
        cache.(age).(days_left) <- result;
        result
      | x -> x)
    else 0
  in
  List.fold ages ~init:0 ~f:(fun count age -> count + 1 + spawn_count age days)
;;

let part1 () =
  let ages = read_ages "input" in
  let count = sim ages 80 in
  Printf.printf "1: %d\n" count
;;

let part2 () =
  let ages = read_ages "input" in
  let count = sim ages 256 in
  Printf.printf "2: %d\n" count
;;

let () =
  part1 ();
  part2 ()
;;
