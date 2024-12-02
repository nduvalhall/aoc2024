open Core

type direction =
  | Increasing
  | Decreasing
  | Level

let get_direction a b =
  match compare a b with
  | x when Int.is_negative x -> Increasing
  | x when Int.is_positive x -> Decreasing
  | _ -> Level
;;

let is_valid a b ~direction =
  match direction with
  | Increasing -> if a < b then if b - a > 3 then false else true else false
  | Decreasing -> if a > b then if a - b > 3 then false else true else false
  | Level -> false
;;

let judge reports =
  let rec aux reports previous direction =
    match reports, previous, direction with
    | [ head ], Some previous, Some direction -> is_valid previous head ~direction
    | head :: tail, None, None -> aux tail (Some head) None
    | head :: tail, Some previous, None ->
      let direction = get_direction previous head in
      if is_valid previous head ~direction
      then aux tail (Some head) (Some (get_direction previous head))
      else false
    | head :: tail, Some previous, Some direction ->
      if is_valid previous head ~direction
      then aux tail (Some head) (Some direction)
      else false
    | _ -> false
  in
  aux reports None None
;;

let remove_at_index l ~index =
  List.mapi ~f:(fun i x -> i, x) l
  |> List.filter ~f:(fun (i, _) -> i <> index)
  |> List.map ~f:snd
;;

let exclude_one_element l =
  let rec aux ~index acc =
    if index >= List.length l
    then acc
    else (
      let sublist = remove_at_index l ~index in
      aux ~index:(index + 1) (sublist :: acc))
  in
  aux ~index:0 []
;;

let () =
  let reports =
    In_channel.read_lines "day2/input.txt"
    |> List.map ~f:(fun s -> String.split ~on:' ' s |> List.map ~f:Int.of_string)
  in
  let part1 =
    reports |> List.map ~f:judge |> List.filter ~f:(fun b -> b) |> List.length
  in
  let part2 =
    reports
    |> List.map ~f:exclude_one_element
    |> List.map ~f:(List.exists ~f:judge)
    |> List.filter ~f:(fun b -> b)
    |> List.length
  in
  Out_channel.print_endline ("Part 1: " ^ Int.to_string part1);
  Out_channel.print_endline ("Part 2: " ^ Int.to_string part2)
;;
