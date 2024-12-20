open Core

let grid =
  In_channel.read_lines "day12/input.txt"
  |> List.to_array
  |> Array.map ~f:String.to_array
;;

let w = Array.length grid.(0)
let h = Array.length grid
let visited = Array.make_matrix ~dimx:w ~dimy:h false
let directions = [ 0, -1; 1, 0; 0, 1; -1, 0 ]
let in_bounds (x, y) = x >= 0 && x < w && y >= 0 && y < h

let rec dfs stack group =
  match stack with
  | [] -> group
  | (x, y) :: rest ->
    if visited.(y).(x)
    then dfs rest group
    else (
      visited.(y).(x) <- true;
      let current_char = grid.(y).(x) in
      let neighbours =
        List.fold directions ~init:[] ~f:(fun acc (dx, dy) ->
          let nx, ny = x + dx, y + dy in
          if in_bounds (nx, ny)
             && (not visited.(ny).(nx))
             && Char.equal current_char grid.(ny).(nx)
          then (nx, ny) :: acc
          else acc)
      in
      dfs (neighbours @ rest) ((x, y) :: group))
;;

let groups =
  let rec aux (x, y) groups =
    if y >= h
    then groups
    else if x >= w
    then aux (0, y + 1) groups
    else if visited.(y).(x)
    then aux (x + 1, y) groups
    else aux (x + 1, y) (dfs [ x, y ] [] :: groups)
  in
  aux (0, 0) []
;;

let areas = List.fold groups ~init:[] ~f:(fun acc g -> List.length g :: acc)

let perimeters =
  List.fold groups ~init:[] ~f:(fun acc g ->
    List.fold g ~init:0 ~f:(fun acc (x, y) ->
      let ns = [ x, y - 1; x + 1, y; x, y + 1; x - 1, y ] in
      let count =
        4
        - List.fold ns ~init:0 ~f:(fun acc (nx, ny) ->
          acc + List.count g ~f:(fun (gx, gy) -> gx = nx && gy = ny))
      in
      count + acc)
    :: acc)
;;

let costs =
  List.fold2 areas perimeters ~init:0 ~f:(fun acc a p -> acc + (a * p))
;;
