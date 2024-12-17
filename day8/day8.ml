open! Core

let antennas =
  In_channel.read_lines "day8/input.txt"
  |> List.mapi ~f:(fun i s ->
    String.to_list s
    |> List.mapi ~f:(fun j c -> c, (i, j))
    |> List.filter ~f:(fun n -> not (Char.equal (fst n) '.')))
  |> List.fold ~init:[] ~f:(fun acc ns ->
    List.fold ns ~init:acc ~f:(fun acc n -> n :: acc))
;;

let find_antinodes (_, (x1, y1)) (_, (x2, y2)) =
  let dx1, dy1 = x1 - x2, y1 - y2
  and dx2, dy2 = x2 - x1, y2 - y1 in
  if (dx1 = 0 && dy1 = 0) || (dx2 = 0 && dy2 = 0)
  then []
  else [ '#', (x1 + dx1, y1 + dy1); '#', (x2 + dx2, y2 + dy2) ]
;;

let find_all_antinodes antennas =
  let rec aux antennas' antinodes =
    match antennas' with
    | [] -> antinodes
    | hd :: tl ->
      let antinodes' =
        List.fold antennas ~init:[] ~f:(fun acc a' ->
          if Char.equal (fst hd) (fst a')
          then find_antinodes hd a' @ acc
          else acc)
      in
      aux tl (antinodes' @ antinodes)
  in
  aux antennas []
;;

let _compare (c1, (x1, y1)) (c2, (x2, y2)) =
  match Int.compare x1 x2 with
  | 0 ->
    (match Int.compare y1 y2 with
     | 0 -> Char.compare c1 c2
     | c -> c)
  | c -> c
;;

let compare_pos (_, (x1, y1)) (_, (x2, y2)) =
  match Int.compare x1 x2 with
  | 0 -> Int.compare y1 y2
  | c -> c
;;

let out_of_bounds w h (_, (x, y)) = x < 0 || x >= w || y < 0 || y >= h

let () =
  let antinodes = find_all_antinodes antennas in
  let antinodes = List.stable_dedup antinodes ~compare:compare_pos in
  let antinodes =
    List.filter antinodes ~f:(fun p -> not (out_of_bounds 50 50 p))
  in
  let all = antennas @ antinodes in
  let ns = List.count all ~f:(fun (c, _) -> Char.equal c '#') in
  printf "Part 1: %d\n" ns
;;
