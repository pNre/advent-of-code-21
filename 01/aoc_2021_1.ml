open Core

let read_ints file = file |> In_channel.read_lines |> List.map ~f:Int.of_string

let count1 depths =
  List.fold2_exn
    (List.drop_last_exn depths)
    (List.tl_exn depths)
    ~init:0
    ~f:(fun acc lhs rhs -> acc + Bool.to_int (lhs < rhs))
;;

let rec count2 depths =
  let sum = List.sum (module Int) ~f:Fn.id in
  let lhs = sum (List.take depths 3)
  and tl = List.drop depths 1 in
  let rhs = sum (List.take tl 3) in
  Bool.to_int (lhs < rhs) + if List.length tl > 3 then count2 tl else 0
;;

let part1 () = "input" |> read_ints |> count1 |> Printf.printf "1: %d\n"
let part2 () = "input" |> read_ints |> count2 |> Printf.printf "2: %d\n"

let () =
  part1 ();
  part2 ()
;;
