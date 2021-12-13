open Core

let read_diag file =
  let bit_of_string = String.equal "1" in
  file
  |> In_channel.read_lines
  |> List.map ~f:String.to_list
  |> List.map ~f:(List.map ~f:(Fn.compose bit_of_string Char.to_string))
;;

let int_of_bits b =
  let len = List.length b in
  List.foldi b ~init:0 ~f:(fun idx accum bit ->
      let idx = len - idx - 1 in
      accum lor (Bool.to_int bit lsl idx))
;;

let power_consumption diags =
  let len = List.length diags in
  let threshold = len / 2 in
  let diags' = List.transpose_exn diags in
  let bits = List.length diags' in
  let gamma, eps =
    List.foldi diags' ~init:(0, 0) ~f:(fun idx (gamma, eps) col ->
        let is_on = List.count col ~f:Fn.id > threshold in
        let bit = 1 lsl (bits - idx - 1) in
        if is_on then gamma lor bit, eps else gamma, eps lor bit)
  in
  gamma * eps
;;

let life_support diags =
  let rec fold ?(bit_pos = 0) compare diags =
    let len = List.length diags in
    let threshold = Float.round_up (len // 2) |> Int.of_float in
    let on_count = List.count diags ~f:(fun bits -> List.nth_exn bits bit_pos) in
    let is_on = compare on_count threshold in
    let xs = List.filter diags ~f:(fun d -> Bool.equal (List.nth_exn d bit_pos) is_on) in
    match xs with
    | [ x ] -> int_of_bits x
    | xs -> fold ~bit_pos:(bit_pos + 1) compare xs
  in
  fold ( >= ) diags * fold ( < ) diags
;;

let part1 () = "input" |> read_diag |> power_consumption |> Printf.printf "1: %d\n"
let part2 () = "input" |> read_diag |> life_support |> Printf.printf "2: %d\n"

let () =
  part1 ();
  part2 ()
;;
