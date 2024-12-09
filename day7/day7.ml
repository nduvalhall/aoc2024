open! Core

let parse_input lines =
  List.map lines ~f:(fun s ->
    let ss = String.split s ~on:':' in
    let result = List.nth_exn ss 0 |> Int.of_string in
    let nums =
      List.nth_exn ss 1
      |> String.split ~on:' '
      |> List.filter ~f:(fun s -> not (String.equal s ""))
      |> List.map ~f:Int.of_string
    in
    result, nums)
;;

let solve nums =
  let rec r l num acc =
    match l with
    | [] -> acc
    | hd :: tl -> r tl num ((hd + num) :: (hd * num) :: acc)
  in
  let rec aux l nums =
    match nums with
    | [] -> l
    | hd :: tl -> aux (r l hd []) tl
  in
  match nums with
  | hd :: tl -> aux [ hd ] tl
  | _ -> []
;;

let solve2 nums =
  let rec r l num acc =
    match l with
    | [] -> acc
    | hd :: tl ->
      let a = hd + num
      and b = hd * num
      and c = Int.of_string (Int.to_string hd ^ Int.to_string num) in
      r tl num (a :: b :: c :: acc)
  in
  let rec aux l nums =
    match nums with
    | [] -> l
    | hd :: tl -> aux (r l hd []) tl
  in
  match nums with
  | hd :: tl -> aux [ hd ] tl
  | _ -> []
;;

let part f input =
  List.map input ~f:(fun x -> fst x, f (snd x))
  |> List.map ~f:(fun x -> fst x, List.exists (snd x) ~f:(fun x' -> x' = fst x))
  |> List.filter ~f:(fun x -> snd x)
  |> List.fold ~init:0 ~f:(fun acc (a, _) -> acc + a)
;;

let () =
  let input = In_channel.read_lines "day7/input.txt" |> parse_input in
  Out_channel.print_endline ("Part 1: " ^ Int.to_string (part solve input));
  Out_channel.print_endline ("Part 2: " ^ Int.to_string (part solve2 input))
;;
