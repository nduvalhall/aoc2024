open Core

let count_list l =
  let rec aux l count =
    match l with
    | [] -> count
    | 'X' :: 'M' :: 'A' :: 'S' :: tl -> aux tl (count + 1)
    | _ :: tl -> aux tl count
  in
  aux l 0
;;

let count_list2 l =
  let rec aux l count =
    match l with
    | [] -> count
    | 'S' :: 'A' :: 'M' :: 'X' :: tl -> aux tl (count + 1)
    | _ :: tl -> aux tl count
  in
  aux l 0
;;

let transpose matrix =
  let rec aux matrix acc =
    match matrix with
    | [] | [] :: _ -> List.rev acc
    | _ ->
      let first_column = List.map ~f:List.hd_exn matrix in
      let rest = List.map ~f:List.tl_exn matrix in
      aux rest (first_column :: acc)
  in
  aux matrix []
;;

let main_diagonals matrix =
  let rows = List.length matrix in
  let cols = List.length (List.hd_exn matrix) in
  let diagonals_from_start start =
    List.foldi matrix ~init:[] ~f:(fun i acc row ->
      let col = start + i in
      if col < cols && col >= 0
      then (
        match List.nth row col with
        | Some x -> x :: acc
        | None -> acc)
      else acc)
    |> List.rev
  in
  let start_from_cols = List.range 0 cols in
  let start_from_rows = List.range 1 rows in
  List.map start_from_cols ~f:(fun c -> diagonals_from_start c)
  @ List.map start_from_rows ~f:(fun r -> diagonals_from_start (-r))
;;

let anti_diagonals matrix =
  let rows = List.length matrix in
  let cols = List.length (List.hd_exn matrix) in
  let diagonals_from_start start =
    List.foldi matrix ~init:[] ~f:(fun i acc row ->
      let col = start - i in
      if col < cols && col >= 0
      then (
        match List.nth row col with
        | Some x -> x :: acc
        | None -> acc)
      else acc)
    |> List.rev
  in
  let start_from_cols = List.range 0 cols in
  let start_from_rows = List.range 1 rows in
  List.map start_from_cols ~f:(fun c -> diagonals_from_start c)
  @ List.map start_from_rows ~f:(fun r -> diagonals_from_start (cols - 1 + r))
;;

let check_area matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let matches_pattern1 x y =
    Char.equal matrix.(x).(y) 'M'
    && Char.equal matrix.(x).(y + 2) 'S'
    && Char.equal matrix.(x + 1).(y + 1) 'A'
    && Char.equal matrix.(x + 2).(y) 'M'
    && Char.equal matrix.(x + 2).(y + 2) 'S'
  in
  let matches_pattern2 x y =
    Char.equal matrix.(x).(y) 'M'
    && Char.equal matrix.(x).(y + 2) 'M'
    && Char.equal matrix.(x + 1).(y + 1) 'A'
    && Char.equal matrix.(x + 2).(y) 'S'
    && Char.equal matrix.(x + 2).(y + 2) 'S'
  in
  let matches_pattern3 x y =
    Char.equal matrix.(x).(y) 'S'
    && Char.equal matrix.(x).(y + 2) 'S'
    && Char.equal matrix.(x + 1).(y + 1) 'A'
    && Char.equal matrix.(x + 2).(y) 'M'
    && Char.equal matrix.(x + 2).(y + 2) 'M'
  in
  let matches_pattern4 x y =
    Char.equal matrix.(x).(y) 'S'
    && Char.equal matrix.(x).(y + 2) 'M'
    && Char.equal matrix.(x + 1).(y + 1) 'A'
    && Char.equal matrix.(x + 2).(y) 'S'
    && Char.equal matrix.(x + 2).(y + 2) 'M'
  in
  let rec count_from x y acc matches_pattern =
    if x > rows - 3
    then acc
    else if y > cols - 3
    then count_from (x + 1) 0 acc matches_pattern
    else
      (let acc = if matches_pattern x y then acc + 1 else acc in
       count_from x (y + 1) acc)
        matches_pattern
  in
  count_from 0 0 0 matches_pattern1
  + count_from 0 0 0 matches_pattern2
  + count_from 0 0 0 matches_pattern3
  + count_from 0 0 0 matches_pattern4
;;

let () =
  let input =
    In_channel.read_lines "day4/input.txt" |> List.map ~f:String.to_list
  in
  let count =
    List.fold ~init:0 ~f:(fun acc x -> acc + count_list x) input
    + List.fold ~init:0 ~f:(fun acc x -> acc + count_list x) (transpose input)
    + List.fold
        ~init:0
        ~f:(fun acc x -> acc + count_list x)
        (main_diagonals input)
    + List.fold
        ~init:0
        ~f:(fun acc x -> acc + count_list x)
        (anti_diagonals input)
    + List.fold ~init:0 ~f:(fun acc x -> acc + count_list2 x) input
    + List.fold ~init:0 ~f:(fun acc x -> acc + count_list2 x) (transpose input)
    + List.fold
        ~init:0
        ~f:(fun acc x -> acc + count_list2 x)
        (main_diagonals input)
    + List.fold
        ~init:0
        ~f:(fun acc x -> acc + count_list2 x)
        (anti_diagonals input)
  in
  Out_channel.print_endline ("Part 1: " ^ Int.to_string count);
  let input =
    In_channel.read_lines "day4/input.txt"
    |> List.to_array
    |> Array.map ~f:String.to_array
  in
  let count = check_area input in
  Out_channel.print_endline ("Part 2: " ^ Int.to_string count)
;;
