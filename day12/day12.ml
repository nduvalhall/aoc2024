open! Core

let parse_input input = List.map input ~f:String.to_array |> List.to_array

let dfs grid graph key =
  Printf.printf "%d, %d\n" (fst key) (snd key);
  let rec dfs_aux key' visited group =
    if Set.mem visited key'
    then visited, group
    else (
      let visited = Set.add visited key' in
      match Graph.find graph key' with
      | None -> visited, group
      | Some neighbours ->
        if Char.equal grid.(fst key).(snd key) grid.(fst key').(snd key')
        then (
          let group = (key', neighbours) :: group in
          List.fold neighbours ~init:(visited, group) ~f:(fun (v, g) key'' ->
            dfs_aux key'' v g))
        else visited, group)
  in
  dfs_aux key (Set.empty (module Graph.Key)) []
;;

let get_groups grid graph =
  let h = Array.length grid in
  let w = if h = 0 then 0 else Array.length grid.(0) in
  let rec groups_aux (i, j) visited groups =
    if i >= h
    then visited, groups
    else if j >= w
    then groups_aux (i + 1, 0) visited groups
    else if Set.mem visited (i, j)
    then groups_aux (i, j + 1) visited groups
    else (
      let visited, group = dfs grid graph (i, j) in
      groups_aux (i, j + 1) visited (group :: groups))
  in
  groups_aux (0, 0) (Set.empty (module Graph.Key)) [] |> snd
;;

let get_perimeter grid group =
  List.fold group ~init:0 ~f:(fun acc ((i, j), ns) ->
    let v = grid.(i).(j) in
    acc + (4 - List.count ns ~f:(fun (i', j') -> Char.equal v grid.(i').(j'))))
;;

let get_price grid group = get_perimeter grid group * List.length group

let () =
  let grid = In_channel.read_lines "day12/test.txt" |> parse_input in
  let graph = Graph.of_grid grid in
  Out_channel.print_endline "";
  let groups = get_groups grid graph in
  let prices = List.map groups ~f:(get_price grid) in
  let part_1 = List.fold ~init:0 prices ~f:( + ) in
  Out_channel.print_endline ("Part 1: " ^ Int.to_string part_1)
;;
