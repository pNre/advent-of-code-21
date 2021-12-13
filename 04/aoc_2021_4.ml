open Core

type board =
  { map : (int * int) Int.Map.t
  ; unmarked_nums : Int.Set.t
  ; rows : int array
  ; cols : int array
  }

let read_input file =
  let lines = In_channel.read_lines file in
  let parse_ints ~sep s =
    s
    |> String.split ~on:sep
    |> List.map ~f:String.strip
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:Int.of_string
  in
  let draw_numbers = List.hd_exn lines |> parse_ints ~sep:',' in
  let boards =
    List.drop lines 2
    |> List.group ~break:(fun a _ -> String.is_empty a)
    |> List.map ~f:(fun board ->
           let layout = List.map board ~f:(parse_ints ~sep:' ') in
           let map =
             List.foldi layout ~init:Int.Map.empty ~f:(fun row_idx map row ->
                 List.foldi row ~init:map ~f:(fun col_idx map num ->
                     Map.set map ~key:num ~data:(row_idx, col_idx)))
           in
           let unmarked_nums = layout |> List.join |> Int.Set.of_list in
           { map
           ; unmarked_nums
           ; rows = Array.create 0 ~len:5
           ; cols = Array.create 0 ~len:5
           })
  in
  draw_numbers, boards
;;

let update_board num board =
  match Int.Map.find board.map num with
  | Some (r, c) ->
    board.rows.(r) <- board.rows.(r) + 1;
    board.cols.(c) <- board.cols.(c) + 1;
    { board with unmarked_nums = Set.remove board.unmarked_nums num }
  | None -> board
;;

let is_winning_board board =
  Array.mem board.rows 5 ~equal:Int.equal || Array.mem board.cols 5 ~equal:Int.equal
;;

let score board num = Set.fold board.unmarked_nums ~init:0 ~f:( + ) * num

let winning_score draw boards =
  List.fold_until
    draw
    ~init:boards
    ~f:(fun boards num ->
      let boards = List.map boards ~f:(update_board num) in
      let winning_board = List.find boards ~f:is_winning_board in
      match winning_board with
      | Some board -> Stop (score board num)
      | None -> Continue boards)
    ~finish:(fun _ -> failwith "No winner")
;;

let last_to_win draw boards =
  List.fold_until
    draw
    ~init:boards
    ~f:(fun boards num ->
      match boards with
      | [ board ] ->
        let board = update_board num board in
        if is_winning_board board then Stop (score board num) else Continue [ board ]
      | boards ->
        let boards = List.map boards ~f:(update_board num) in
        let non_winning_boards = List.filter boards ~f:(Fn.non is_winning_board) in
        Continue non_winning_boards)
    ~finish:(fun _ -> failwith "No winner")
;;

let part1 () =
  let draw, boards = read_input "input" in
  winning_score draw boards |> Printf.printf "1: %d\n"
;;

let part2 () =
  let draw, boards = read_input "input" in
  last_to_win draw boards |> Printf.printf "2: %d\n"
;;

let () =
  part1 ();
  part2 ()
;;
