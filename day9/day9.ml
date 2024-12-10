open! Core

type t =
  | File
  | Empty

let empty n = List.init n ~f:(fun _ -> -1)
let file id n = List.init n ~f:(fun _ -> id)

let explode blocks =
  let rec aux blocks i t acc =
    match blocks, t with
    | [], _ -> acc
    | hd :: tl, Empty -> aux tl i File (acc @ empty hd)
    | hd :: tl, File -> aux tl (i + 1) Empty (acc @ file i hd)
  in
  aux blocks 0 File []
;;

let compress blocks =
  let to_fill = List.count blocks ~f:(fun x -> x = -1) in
  let rec aux n front back acc =
    match n, front, back with
    | 0, _, hd :: _ -> List.rev (hd :: acc)
    | _, h1 :: t1, h2 :: t2 ->
      if h1 = -1
      then if h2 = -1 then aux (n - 1) front t2 acc else aux (n - 1) t1 t2 (h2 :: acc)
      else aux n t1 back (h1 :: acc)
    | _ -> List.rev acc
  in
  aux to_fill blocks (List.rev blocks) []
;;

let checksum blocks =
  List.foldi blocks ~init:0 ~f:(fun i acc x -> if x = -1 then acc else acc + (i * x))
;;

let () =
  let input =
    In_channel.read_all "day9/input.txt"
    |> String.rstrip
    |> String.to_list
    |> List.map ~f:Char.get_digit_exn
  in
  let part_1 = input |> explode |> compress |> checksum in
  Out_channel.print_endline ("Part 1: " ^ Int.to_string part_1)
;;
