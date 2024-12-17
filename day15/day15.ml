open! Core

let start grid =
  match
    grid
    |> Array.foldi ~init:[] ~f:(fun i acc a ->
      Array.foldi a ~init:acc ~f:(fun j acc b -> (b, (i, j)) :: acc))
    |> List.find ~f:(fun (c, _) -> Char.equal c '@')
  with
  | None -> failwith "Robot not found"
  | Some (_, r) -> r
;;

let grid, h, w, graph, instructions =
  let lines = In_channel.read_lines "day15/input.txt" in
  let grid =
    List.take_while lines ~f:(fun s -> not (String.is_empty s))
    |> List.to_array
    |> Array.map ~f:String.to_array
  in
  let h, w = Array.length grid, Array.length grid.(0) in
  let graph = Graph.of_grid grid in
  let instructions =
    List.rev lines
    |> List.take_while ~f:(fun s -> not (String.is_empty s))
    |> List.rev
    |> List.fold ~init:[] ~f:(fun acc s ->
      String.to_list s
      |> List.fold ~init:acc ~f:(fun acc c ->
        (let open Direction in
         match c with
         | '^' -> Up
         | '>' -> Right
         | 'v' -> Down
         | '<' -> Left
         | c -> failwith (sprintf "Unknown direction %c" c))
        :: acc))
    |> List.rev
  in
  grid, h, w, graph, instructions
;;

let peek grid (x, y) direction =
  let open Direction in
  let x', y' = translate direction (x, y) in
  if x' < 0
     || y' < 0
     || x' >= w
     || y' >= h
     || Char.equal grid.(x').(y') '#'
     || Char.equal grid.(x').(y') '.'
  then None
  else Some (x', y')
;;

let to_move grid (x, y) direction =
  let open Direction in
  let rec aux (x', y') moves =
    let x'', y'' = translate direction (x', y') in
    if Char.equal grid.(x'').(y'') '#'
    then []
    else if Char.equal grid.(x'').(y'') '.'
    then (x', y') :: moves
    else aux (x'', y'') ((x', y') :: moves)
  in
  aux (x, y) []
;;

let move grid moves direction =
  let rec aux moves grid =
    match moves with
    | [] -> grid
    | (x, y) :: tl ->
      let x', y' = Direction.translate direction (x, y) in
      let () = grid.(x').(y') <- grid.(x).(y) in
      let () = grid.(x).(y) <- '.' in
      aux tl grid
  in
  aux moves grid
;;

let move_all grid instructions =
  let rec aux grid instructions' =
    match instructions' with
    | [] -> grid
    | hd :: tl ->
      let moves = to_move grid (start grid) hd in
      let grid = move grid moves hd in
      aux grid tl
  in
  aux grid instructions
;;

let part_one =
  let grid = move_all grid instructions in
  Array.foldi grid ~init:[] ~f:(fun i acc r ->
    Array.foldi r ~init:acc ~f:(fun j acc c ->
      if Char.equal c 'O' then ((i * 100) + j) :: acc else acc))
  |> List.fold ~init:0 ~f:(fun acc x -> acc + x)
;;
