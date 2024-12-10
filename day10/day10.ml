open! Core

let parse_input lines =
  lines
  |> List.to_array
  |> Array.map ~f:(fun s -> String.to_array s |> Array.map ~f:Char.get_digit_exn)
;;

let next grid (i, j) =
  let h, w = Array.length grid, Array.length grid.(0) in
  let check (i, j) (i', j') = if grid.(i').(j') - grid.(i).(j) = 1 then true else false in
  let up =
    if i <= 0 then None else if check (i, j) (i - 1, j) then Some (i - 1, j) else None
  and right =
    if j + 1 >= w then None else if check (i, j) (i, j + 1) then Some (i, j + 1) else None
  and down =
    if i + 1 >= h then None else if check (i, j) (i + 1, j) then Some (i + 1, j) else None
  and left =
    if j <= 0 then None else if check (i, j) (i, j - 1) then Some (i, j - 1) else None
  in
  [ up; right; down; left ]
  |> List.filter ~f:Option.is_some
  |> List.map ~f:(fun o -> Option.value_exn o)
;;

let climb grid start =
  let compare_pos (i, j) (i', j') =
    match Int.compare i i' with
    | 0 -> Int.compare j j'
    | cmp -> cmp
  in
  let rec climb_aux grid start acc =
    match start with
    | [] -> acc
    | _ ->
      let rec next_aux a's acc =
        match a's with
        | [] -> acc
        | hd :: tl -> next_aux tl (next grid hd @ acc)
      in
      let next = next_aux start [] in
      climb_aux grid next (start @ acc)
  in
  climb_aux grid [ start ] []
  |> List.dedup_and_sort ~compare:compare_pos
  |> List.map ~f:(fun (i, j) -> grid.(i).(j))
  |> List.count ~f:(fun x -> x = 9)
;;

let collect_starts grid =
  let h, w = Array.length grid, Array.length grid.(0) in
  let rec aux i j acc =
    if i >= h
    then acc
    else if j >= w
    then aux (i + 1) 0 acc
    else if grid.(i).(j) = 0
    then aux i (j + 1) ((i, j) :: acc)
    else aux i (j + 1) acc
  in
  aux 0 0 []
;;

let part_1 =
  let grid = In_channel.read_lines "day10/input.txt" |> parse_input in
  grid
  |> collect_starts
  |> List.map ~f:(fun s -> climb grid s)
  |> List.fold ~init:0 ~f:(fun acc count -> acc + count)
;;

let () = Out_channel.print_endline (Int.to_string part_1)
