open Core

let read_commands file =
  file
  |> In_channel.read_lines
  |> List.map ~f:(fun line ->
         let cmd, value = String.lsplit2_exn line ~on:' ' in
         let value = Int.of_string value in
         match cmd with
         | "forward" -> `F value
         | "up" -> `D (-value)
         | "down" -> `D value
         | t -> failwithf "Unexpected token %s" t ())
;;

let count1 cmds =
  let d, h =
    List.fold cmds ~init:(0, 0) ~f:(fun (d, h) cmd ->
        match cmd with
        | `F v -> d, h + v
        | `D v -> d + v, h)
  in
  d * h
;;

let count2 cmds =
  let d, h, _ =
    List.fold cmds ~init:(0, 0, 0) ~f:(fun (d, h, a) cmd ->
        match cmd with
        | `F v -> d + (a * v), h + v, a
        | `D v -> d, h, a + v)
  in
  d * h
;;

let part1 () = "in.txt" |> read_commands |> count1 |> Printf.printf "1: %d\n"
let part2 () = "in.txt" |> read_commands |> count2 |> Printf.printf "2: %d\n"

let () =
  part1 ();
  part2 ()
;;
