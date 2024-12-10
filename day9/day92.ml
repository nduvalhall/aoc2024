open! Core

type t =
  | File of int * int
  | Empty of int

let parse_input s =
  let l = s |> String.rstrip |> String.to_list |> List.map ~f:Char.get_digit_exn in
  let rec aux l i id acc =
    match l with
    | [] -> acc
    | hd :: tl ->
      if i % 2 = 0
      then (
        let f = File (id, hd) in
        aux tl (i + 1) (id + 1) (f :: acc))
      else (
        let e = Empty hd in
        aux tl (i + 1) id (e :: acc))
  in
  aux l 0 0 [] |> List.rev
;;

let compress blocks =
  let rec aux blocks =
    let block = List.rev blocks |> List.hd_exn
    and blocks = List.rev blocks |> List.tl_exn |> List.rev in
    place blocks block
  in
  aux blocks
;;

let checksum blocks =
  List.foldi blocks ~init:0 ~f:(fun i acc x -> if x = -1 then acc else acc + (i * x))
;;

let () =
  let input = In_channel.read_all "day9/input.txt" |> parse_input in
  let part_1 = input |> explode |> compress |> checksum in
  Out_channel.print_endline ("Part 1: " ^ Int.to_string part_1)
;;
