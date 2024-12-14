open! Core

let parse_input lines =
  let parse_block b =
    let b =
      List.map b ~f:(fun l ->
        l
        |> String.substr_replace_all ~pattern:"Button A: " ~with_:""
        |> String.substr_replace_all ~pattern:"Button B: " ~with_:""
        |> String.substr_replace_all ~pattern:"Prize: " ~with_:""
        |> String.substr_replace_all ~pattern:"X+" ~with_:""
        |> String.substr_replace_all ~pattern:"Y+" ~with_:""
        |> String.substr_replace_all ~pattern:"X=" ~with_:""
        |> String.substr_replace_all ~pattern:"Y=" ~with_:""
        |> String.substr_replace_all ~pattern:", " ~with_:"_"
        |> String.split ~on:'_'
        |> List.map ~f:Int.of_string)
    in
    let l1 = List.nth_exn b 0
    and l2 = List.nth_exn b 1
    and l3 = List.nth_exn b 2 in
    let a1 = List.nth_exn l1 0
    and a2 = List.nth_exn l2 0
    and a = List.nth_exn l3 0
    and b1 = List.nth_exn l1 1
    and b2 = List.nth_exn l2 1
    and b = List.nth_exn l3 1 in
    (a1, a2, a), (b1, b2, b)
  in
  List.filter lines ~f:(fun l -> not (String.equal l ""))
  |> List.chunks_of ~length:3
  |> List.map ~f:parse_block
;;

let solve ((a1, a2, a), (b1, b2, b)) ~offset =
  let a1, a2, a = Int.to_float a1, Int.to_float a2, Int.to_float (a + offset)
  and b1, b2, b = Int.to_float b1, Int.to_float b2, Int.to_float (b + offset) in
  let det_a = (a1 *. b2) -. (a2 *. b1) in
  if Float.compare det_a 0. = 0
  then None
  else (
    let x = ((b2 *. a) -. (a2 *. b)) /. det_a in
    let y = ((a1 *. b) -. (b1 *. a)) /. det_a in
    if Float.is_integer x && Float.is_integer y
    then Some (Float.to_int x, Float.to_int y)
    else None)
;;

let () =
  let eqs = In_channel.read_lines "day13/input.txt" |> parse_input in
  let part1 =
    List.map eqs ~f:(solve ~offset:0)
    |> List.fold ~init:0 ~f:(fun acc s ->
      match s with
      | None -> acc
      | Some s -> acc + (fst s * 3) + snd s)
  in
  let part2 =
    List.map eqs ~f:(solve ~offset:10000000000000)
    |> List.fold ~init:0 ~f:(fun acc s ->
      match s with
      | None -> acc
      | Some s -> acc + (fst s * 3) + snd s)
  in
  Printf.printf "Part 1: %d\n" part1;
  Printf.printf "Part 2: %d\n" part2
;;
