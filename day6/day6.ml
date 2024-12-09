open! Core

module Direction = struct
  type t =
    | Up
    | Right
    | Down
    | Left

  let next d =
    match d with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up
  ;;
end

type cell =
  { char : char
  ; visited : int
  ; direction : Direction.t
  }

let find_guard grid =
  let h = Array.length grid
  and w = Array.length grid.(0) in
  let rec find_guard_aux i j =
    if i >= h
    then raise (Failure "Failed to find guard.")
    else if j >= w
    then find_guard_aux (i + 1) 0
    else if Char.equal grid.(i).(j).char '^'
    then i, j
    else find_guard_aux i (j + 1)
  in
  find_guard_aux 0 0
;;

let next_cell grid i j direction =
  let h = Array.length grid
  and w = Array.length grid.(0) in
  match i >= h || j >= w with
  | true -> None
  | false ->
    let i', j' =
      match direction with
      | Direction.Up -> i - 1, j
      | Direction.Right -> i, j + 1
      | Direction.Down -> i + 1, j
      | Direction.Left -> i, j - 1
    in
    (match i' >= h || j' >= w || i' < 0 || j' < 0 with
     | true -> None
     | false ->
       (match grid.(i').(j').char with
        | '#' -> Some (i, j, Direction.next direction)
        | _ -> Some (i', j', direction)))
;;

let traverse grid i j =
  let rec traverse_aux i j direction count =
    if grid.(i).(j).visited = 5
    then None
    else (
      match next_cell grid i j direction with
      | None -> Some count
      | Some (i', j', direction) ->
        (match grid.(i).(j).visited with
         | 0 ->
           grid.(i).(j) <- { (grid.(i).(j)) with visited = grid.(i).(j).visited + 1 };
           traverse_aux i' j' direction (count + 1)
         | _ ->
           grid.(i).(j) <- { (grid.(i).(j)) with visited = grid.(i).(j).visited + 1 };
           traverse_aux i' j' direction count))
  in
  traverse_aux i j Direction.Up 1
;;

let copy grid = Array.map grid ~f:Array.copy

let grids_of_grid grid =
  let h = Array.length grid
  and w = Array.length grid.(0) in
  let rec aux i j grids =
    if i >= h
    then grids
    else if j >= w
    then aux (i + 1) 0 grids
    else (
      let copy = copy grid in
      copy.(i).(j) <- { (copy.(i).(j)) with char = '#' };
      aux i (j + 1) (copy :: grids))
  in
  aux 0 0 []
;;

let () =
  let grid =
    In_channel.read_lines "day6/input.txt"
    |> List.to_array
    |> Array.map ~f:(fun s ->
      String.to_array s
      |> Array.map ~f:(fun c -> { char = c; visited = 0; direction = Up }))
  in
  let i, j = find_guard grid in
  let part_1 = Option.value_exn (traverse (copy grid) i j) in
  let grids = grids_of_grid (copy grid) in
  let part_2 =
    List.map grids ~f:(fun g -> traverse (copy g) i j) |> List.count ~f:Option.is_none
  in
  Out_channel.print_endline ("Part 1: " ^ Int.to_string part_1);
  Out_channel.print_endline ("Part 2: " ^ Int.to_string part_2)
;;
