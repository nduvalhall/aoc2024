open! Core

let parse_input s = s |> String.rstrip |> String.to_list |> List.map ~f:Char.get_digit_exn

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
  let count = List.count blocks ~f:(fun x -> x <> -1) in
  let nums = List.filter blocks ~f:(fun x -> x <> -1) |> List.rev in
  let rec aux a b count acc =
    match count with
    | 0 -> acc
    | _ ->
      (match a with
       | [] -> acc
       | -1 :: tl ->
         (match b with
          | [] -> acc
          | hb :: tb -> aux tl tb (count - 1) (hb :: acc))
       | hd :: tl -> aux tl b (count - 1) (hd :: acc))
  in
  aux blocks nums count [] |> List.rev
;;

let checksum blocks =
  List.foldi blocks ~init:0 ~f:(fun i acc x -> if x = -1 then acc else acc + (i * x))
;;

let () =
  let input = In_channel.read_all "day9/input.txt" |> parse_input in
  let part_1 = input |> explode |> compress |> checksum in
  Out_channel.print_endline ("Part 1: " ^ Int.to_string part_1)
;;
