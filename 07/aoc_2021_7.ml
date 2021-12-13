open Core

let read_positions file =
  file |> In_channel.read_all |> String.split ~on:',' |> List.map ~f:Int.of_string
;;

let fuel1 positions =
  let sorted = List.sort positions ~compare:Int.compare in
  let mid = List.length sorted / 2 in
  let target = List.nth_exn sorted mid in
  List.fold positions ~init:0 ~f:(fun fuel position -> fuel + Int.abs (position - target))
;;

let fuel2 crab_positions =
  let max = List.max_elt crab_positions ~compare:Int.compare |> Option.value_exn in
  let possible_positions = List.init (max + 1) ~f:Fn.id in
  let cost_to_position a b =
    let k = Int.abs (a - b) in
    k * (k + 1) / 2
  in
  snd
    (List.fold
       possible_positions
       ~init:(0, Int.max_value)
       ~f:(fun (best_position, best_cost) position ->
         let cost =
           List.fold crab_positions ~init:0 ~f:(fun cost crab_position ->
               cost + cost_to_position crab_position position)
         in
         if cost < best_cost then position, cost else best_position, best_cost))
;;

let part1 () =
  let positions = read_positions "input" in
  Printf.printf "1: %d\n" (fuel1 positions)
;;

let part2 () =
  let positions = read_positions "input" in
  Printf.printf "2: %d\n" (fuel2 positions)
;;

let () =
  part1 ();
  part2 ()
;;
