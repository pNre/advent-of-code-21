open Core

type operator_type =
  | Sum
  | Product
  | Min
  | Max
  | Gt
  | Lt
  | Eq

let operator_type_of_int = function
  | 0 -> Sum
  | 1 -> Product
  | 2 -> Min
  | 3 -> Max
  | 5 -> Gt
  | 6 -> Lt
  | 7 -> Eq
  | op -> failwithf "Unsupported operator: %d" op ()
;;

type packet =
  { version : int
  ; typ : packet_type
  }

and packet_type =
  | Literal of int64
  | Operator of (operator_type * packet list)

let read_transmission file =
  file
  |> In_channel.read_all
  |> String.to_list
  |> List.groupi ~break:(fun i _ _ -> i % 2 = 0)
  |> List.map ~f:(fun ch ->
         ch
         |> String.of_char_list
         |> Fn.compose Int.Hex.of_string (( ^ ) "0x")
         |> Char.of_int_exn)
  |> String.of_char_list
  |> Bitstring.bitstring_of_string
;;

let parse_packets transmission =
  let rec parse_packets transmission packets limit =
    let parse_literal transmission =
      let rec parse_literal transmission literal =
        match%bitstring transmission with
        | {| true : 1; bits : 4 : bitstring; transmission : -1 : bitstring |} ->
          parse_literal transmission (Bitstring.concat [ literal; bits ])
        | {| false : 1; bits : 4 : bitstring; transmission : -1 : bitstring |} ->
          let literal = Bitstring.concat [ literal; bits ] in
          let length = Bitstring.bitstring_length literal in
          let literal =
            match%bitstring literal with
            | {| literal : length : int |} -> literal
            | {| _ |} -> failwithf "Unsupported %d-bits literal" length ()
          in
          let transmission = Bitstring.dropbits (length % 4) transmission in
          transmission, literal
        | {| _ |} -> failwith "Malformed literal"
      in
      parse_literal transmission Bitstring.empty_bitstring
    in
    let parse_subpackets_by_length transmission =
      match%bitstring transmission with
      | {| length : 15; transmission : -1 : bitstring |} ->
        let sub_transmission = Bitstring.takebits length transmission in
        let _, sub_packets = parse_packets sub_transmission [] 0 in
        Bitstring.dropbits length transmission, sub_packets
      | {| _ |} -> failwith "Malformed operator"
    in
    let parse_subpackets_by_number_of_packets transmission =
      match%bitstring transmission with
      | {| limit : 11; transmission : -1 : bitstring |} ->
        parse_packets transmission [] limit
      | {| _ |} -> failwith "Malformed operator"
    in
    if limit > 0 && List.length packets = limit
    then transmission, packets
    else (
      match%bitstring transmission with
      | {| version : 3; 4 : 3; transmission : -1 : bitstring |} ->
        let transmission, literal = parse_literal transmission in
        parse_packets transmission ({ version; typ = Literal literal } :: packets) limit
      | {| _ |} as transmission when Bitstring.is_zeroes_bitstring transmission ->
        transmission, packets
      | {| version : 3; type_id : 3; false : 1; transmission : -1 : bitstring |} ->
        let transmission, sub_packets = parse_subpackets_by_length transmission in
        parse_packets
          transmission
          ({ version
           ; typ = Operator (operator_type_of_int type_id, List.rev sub_packets)
           }
          :: packets)
          limit
      | {| version : 3; type_id : 3; true : 1; transmission : -1 : bitstring |} ->
        let transmission, sub_packets =
          parse_subpackets_by_number_of_packets transmission
        in
        parse_packets
          transmission
          ({ version
           ; typ = Operator (operator_type_of_int type_id, List.rev sub_packets)
           }
          :: packets)
          limit
      | {| _ |} -> failwith "Malformed packet")
  in
  let _, packets = parse_packets transmission [] 0 in
  packets
;;

let rec sum_versions packets =
  match packets with
  | [] -> 0
  | { version; typ = Literal _ } :: packets -> version + sum_versions packets
  | { version; typ = Operator (_, sub_packets) } :: packets ->
    version + sum_versions sub_packets + sum_versions packets
;;

let apply_operator op values =
  let open Int64 in
  match op with
  | Sum -> List.sum (module Int64) values ~f:Fn.id
  | Product -> List.fold ~init:one values ~f:( * )
  | Min -> Option.value_exn (List.min_elt values ~compare)
  | Max -> Option.value_exn (List.max_elt values ~compare)
  | Gt -> if List.hd_exn values > List.last_exn values then one else zero
  | Lt -> if List.hd_exn values < List.last_exn values then one else zero
  | Eq -> if List.hd_exn values = List.last_exn values then one else zero
;;

let rec compute packets =
  match packets with
  | [] -> []
  | { typ = Literal l; _ } :: packets -> l :: compute packets
  | { typ = Operator (op, sub_packets); _ } :: packets ->
    apply_operator op (compute sub_packets) :: compute packets
;;

let part1 () =
  let transmission = read_transmission "input" in
  let packets = parse_packets transmission in
  Printf.printf "1: %d\n" (sum_versions packets)
;;

let part2 () =
  let transmission = read_transmission "input" in
  let packets = parse_packets transmission in
  Printf.printf
    "2: %s\n"
    (packets |> compute |> List.map ~f:Int64.to_string |> String.concat ~sep:"")
;;

let () =
  part1 ();
  part2 ()
;;
