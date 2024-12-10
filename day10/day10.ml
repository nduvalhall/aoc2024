open! Core

let map = "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732"

let parse_input input =
  String.split input ~on:'\n'
  |> List.to_array
  |> Array.map ~f:(fun s ->
    String.to_array s
    |> Array.map ~f:(fun c -> if Char.equal c '.' then -1 else Char.get_digit_exn c))
;;

let map = parse_input map
let dimensions map = Array.length map, Array.length map.(0)

let get_nexts grid pos =
  let i, j = pos in
  let h, w = dimensions grid
  and current = grid.(i).(j) in
  let up =
    if i <= 0
    then None
    else if grid.(i - 1).(j) - current = 1
    then Some (i - 1, j)
    else None
  and right =
    if j >= w
    then None
    else if grid.(i).(j + 1) - current = 1
    then Some (i, j + 1)
    else None
  and down =
    if i >= h
    then None
    else if grid.(i + 1).(j) - current = 1
    then Some (i + 1, j)
    else None
  and left =
    if j <= 0
    then None
    else if grid.(i).(j - 1) - current = 1
    then Some (i, j - 1)
    else None
  in
  [ up; right; down; left ]
;;

let rec climb next acc =
  let rec aux nexts acc =
      match nexts with
      | [] -> acc
      | None::tl -> aux tl acc
      | hd::tl -> aux tl (hd::acc) 
in  
;;
