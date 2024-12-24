open Core

let secrets =
  In_channel.read_lines "day22/input.txt" |> List.map ~f:Int.of_string
;;

let secret s =
  let a = Int.bit_xor (s * 64) s % 16777216 in
  let b = Int.bit_xor (a / 32) a % 16777216 in
  let c = Int.bit_xor (b * 2048) b % 16777216 in
  c
;;

let rec nsecret n s =
  match n with
  | 0 -> s
  | _ -> nsecret (n - 1) (secret s)
;;

let rec find n s (pa, pb, pc, pd) (a, b, c, d) p =
  match n with
  | 0 -> None
  | _ ->
    if a = pa && b = pb && c = pc && d = pd
    then Some p
    else (
      let s' = secret s in
      find (n - 1) s' (pa, pb, pc, pd) (b, c, d, (s' % 10) - (s % 10)) (s' % 10))
;;

let part1 =
  List.map secrets ~f:(nsecret 2000)
  |> List.fold ~init:0 ~f:(fun acc s -> s + acc)
;;

let part2 =
  List.map secrets ~f:(fun s ->
    find
      2000
      s
      (-2, 1, -1, 3)
      (Int.max_value, Int.max_value, Int.max_value, Int.max_value)
      0)
  |> List.fold ~init:0 ~f:(fun acc p ->
    match p with
    | Some p -> acc + p
    | None -> acc)
;;
