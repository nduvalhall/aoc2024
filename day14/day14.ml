open! Core

let parse_input lines =
  List.map lines ~f:(fun s ->
    let l =
      s
      |> String.substr_replace_all ~pattern:"p=" ~with_:""
      |> String.substr_replace_all ~pattern:"v=" ~with_:""
      |> String.split ~on:' '
      |> List.map ~f:(fun s ->
        let l = s |> String.split ~on:',' |> List.map ~f:Int.of_string in
        List.nth_exn l 0, List.nth_exn l 1)
    in
    let p = List.nth_exn l 0
    and v = List.nth_exn l 1 in
    p, v)
;;

let step w h ((x, y), (vx, vy)) =
  let x =
    if x + vx >= w
    then x + vx - w
    else if x + vx < 0
    then (((x + vx) % w) + w) % w
    else x + vx
  and y =
    if y + vy >= h
    then y + vy - h
    else if y + vy < 0
    then (((y + vy) % h) + h) % h
    else y + vy
  in
  (x, y), (vx, vy)
;;

let stepn w h n robots =
  let rec aux n robots =
    if n = 0
    then robots
    else (
      let robots = List.map robots ~f:(fun r -> step w h r) in
      aux (n - 1) robots)
  in
  aux n robots
;;

let robots_in_quadrant (x, y) w h robots =
  List.fold robots ~init:0 ~f:(fun acc ((xr, yr), _) ->
    if xr >= x && xr < x + w && yr >= y && yr < y + h then acc + 1 else acc)
;;

let safety_factor w h robots =
  let qw = w / 2
  and qh = h / 2 in
  let qs = [ 0, 0; qw + 1, 0; 0, qh + 1; qw + 1, qh + 1 ] in
  List.fold qs ~init:1 ~f:(fun acc q ->
    let rs = robots_in_quadrant q qw qh robots in
    if rs = 0 then acc else acc * rs)
;;

let find_3x3 robots =
  let check_robot (x, y) set =
    List.for_all
      [ x, y
      ; x + 1, y
      ; x + 2, y
      ; x, y + 1
      ; x + 1, y + 1
      ; x + 2, y + 1
      ; x, y + 2
      ; x + 1, y + 2
      ; x + 2, y + 2
      ]
      ~f:(fun r ->
        List.exists set ~f:(fun (r', _) -> fst r = fst r' && snd r = snd r'))
  in
  let rec find robots set =
    match robots with
    | [] -> None
    | (robot, _) :: rest ->
      if check_robot robot set then Some robot else find rest set
  in
  find robots robots
;;

let () =
  let () =
    In_channel.read_lines "day14/input.txt"
    |> parse_input
    |> stepn 101 103 100
    |> safety_factor 101 103
    |> printf "Part 1: %d\n"
  in
  let () =
    let robots = In_channel.read_lines "day14/input.txt" |> parse_input in
    let rec part2 robots i max =
      match i with
      | _ when i = max -> None
      | _ ->
        (match find_3x3 robots with
         | None -> part2 (stepn 101 103 1 robots) (i + 1) max
         | _ -> Some i)
    in
    let r = part2 robots 0 7000 in
    match r with
    | None -> printf "Part 2: None found\n"
    | Some r -> printf "Part 2: %d\n" r
  in
  ()
;;
