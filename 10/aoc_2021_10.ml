open Core

let read_nav file = file |> In_channel.read_lines |> List.map ~f:String.to_list

let close_token = function
  | '[' -> ']'
  | '(' -> ')'
  | '{' -> '}'
  | '<' -> '>'
  | token -> failwithf "Unexpected open token: %s" (String.of_char token) ()
;;

let open_token = function
  | ')' -> '('
  | ']' -> '['
  | '}' -> '{'
  | '>' -> '<'
  | token -> failwithf "Unexpected close token: %s" (String.of_char token) ()
;;

let points_of_illegal_token = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | token -> failwithf "Unexpected illegal token: %s" (String.of_char token) ()
;;

let points_of_close_token = function
  | ')' -> 1
  | ']' -> 2
  | '}' -> 3
  | '>' -> 4
  | token -> failwithf "Unexpected close token: %s" (String.of_char token) ()
;;

let part1 () =
  let navigation_subsystem = read_nav "input" in
  navigation_subsystem
  |> List.map ~f:(fun line ->
         let stack = Stack.create () in
         List.find_map line ~f:(fun ch ->
             match ch with
             | '{' | '[' | '(' | '<' ->
               Stack.push stack ch;
               None
             | '}' | ']' | ')' | '>' ->
               (match Stack.pop stack with
               | Some x when Char.equal x (open_token ch) -> None
               | Some _ -> Some (points_of_illegal_token ch)
               | None -> None)
             | token -> failwithf "Unexpected token: %s" (String.of_char token) ())
         |> Option.value ~default:0)
  |> List.fold ~init:0 ~f:( + )
  |> Printf.printf "1: %d\n"
;;

let part2 () =
  let navigation_subsystem = read_nav "input" in
  let scores =
    navigation_subsystem
    |> List.filter_map ~f:(fun line ->
           let stack = Stack.create () in
           let is_illegal =
             List.exists line ~f:(fun ch ->
                 match ch with
                 | '{' | '[' | '(' | '<' ->
                   Stack.push stack ch;
                   false
                 | '}' | ']' | ')' | '>' ->
                   (match Stack.pop stack with
                   | Some x -> not (Char.equal x (open_token ch))
                   | None -> false)
                 | token -> failwithf "Unexpected token: %s" (String.of_char token) ())
           in
           if is_illegal
           then None
           else
             stack
             |> Stack.to_list
             |> List.map ~f:close_token
             |> List.map ~f:points_of_close_token
             |> List.fold ~init:0 ~f:(fun total score -> (total * 5) + score)
             |> Option.return)
    |> List.sort ~compare:Int.compare
  in
  Printf.printf "2: %d\n" (List.nth_exn scores (List.length scores / 2))
;;

let () =
  part1 ();
  part2 ()
;;
