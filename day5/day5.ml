open Core

let is_a_before_b rules a b =
  match List.find rules ~f:(fun x -> fst x = a && snd x = b) with
  | Some _ -> true
  | None -> false
;;

let is_a_after_b rules a b =
  match List.find rules ~f:(fun x -> snd x = a && fst x = b) with
  | Some _ -> true
  | None -> false
;;

let split l x =
  let rec get_left_aux l acc =
    match l with
    | [] -> List.rev acc, []
    | hd :: tl ->
      if hd = x then List.rev acc, tl else get_left_aux tl (hd :: acc)
  in
  get_left_aux l []
;;

let is_page_valid rules update page =
  let before, after = split update page in
  let rec validate_before before =
    match before with
    | [] -> true
    | hd :: tl ->
      (match is_a_before_b rules hd page with
       | true -> validate_before tl
       | false -> false)
  in
  let rec validate_after after =
    match after with
    | [] -> true
    | hd :: tl ->
      (match is_a_after_b rules hd page with
       | true -> validate_after tl
       | false -> false)
  in
  validate_before before && validate_after after
;;

let is_update_valid rules update =
  let rec is_update_valid_aux update_aux =
    match update_aux with
    | [] -> true
    | hd :: tl ->
      (match is_page_valid rules update hd with
       | true -> is_update_valid_aux tl
       | false -> false)
  in
  is_update_valid_aux update
;;

let get_middle update = List.drop update (List.length update / 2) |> List.hd_exn

let drop_until l s =
  let rec drop_until_aux l =
    match l with
    | [] -> []
    | hd :: tl -> if String.equal hd s then tl else drop_until_aux tl
  in
  drop_until_aux l
;;

let sort_update rules (update : int list) =
  List.sort update ~compare:(fun a b ->
    if is_a_after_b rules a b then 1 else -1)
;;

let () =
  let lines = In_channel.read_lines "day5/input.txt" in
  let rules =
    List.take_while lines ~f:(fun s -> not (String.equal s ""))
    |> List.map ~f:(fun s ->
      let l = String.split s ~on:'|' in
      Int.of_string (List.nth_exn l 0), Int.of_string (List.nth_exn l 1))
  in
  let updates =
    drop_until lines ""
    |> List.map ~f:(fun s ->
      String.split s ~on:',' |> List.map ~f:(fun s -> Int.of_string s))
  in
  let part1 =
    List.map updates ~f:(fun update ->
      if is_update_valid rules update then get_middle update else -1)
    |> List.filter ~f:(fun x -> x <> -1)
    |> List.fold ~init:0 ~f:(fun acc x -> acc + x)
  in
  let part2 =
    List.map updates ~f:(fun update ->
      if not (is_update_valid rules update)
      then sort_update rules update |> get_middle
      else -1)
    |> List.filter ~f:(fun x -> x <> -1)
    |> List.fold ~init:0 ~f:(fun acc x -> acc + x)
  in
  Out_channel.print_endline ("Part 1: " ^ Int.to_string part1);
  Out_channel.print_endline ("Part 2: " ^ Int.to_string part2)
;;
