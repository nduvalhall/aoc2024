open Core

type instruction =
  | Mul of int * int
  | Do
  | Don't

let instruction_of_string s =
  match s with
  | "do()" -> Do
  | "don't()" -> Don't
  | _ ->
    let values =
      String.substr_replace_all s ~pattern:"mul(" ~with_:""
      |> String.substr_replace_all ~pattern:")" ~with_:""
      |> String.split ~on:','
      |> List.map ~f:Int.of_string
    in
    Mul (List.nth_exn values 0, List.nth_exn values 1)
;;

let parse input =
  let l = String.to_list input in
  let rec aux l prev results acc =
    match l with
    | [] -> List.rev results
    | hd :: tl ->
      (match prev, hd with
       | None, 'd' -> aux tl (Some hd) results (hd :: acc)
       | Some 'd', 'o' -> aux tl (Some hd) results (hd :: acc)
       | Some 'o', '(' -> aux tl (Some hd) results (hd :: acc)
       | Some '(', ')' ->
         let instruction =
           List.rev (hd :: acc) |> String.of_list |> instruction_of_string
         in
         aux tl None (instruction :: results) []
       | Some 'o', 'n' -> aux tl (Some hd) results (hd :: acc)
       | Some 'n', '\'' -> aux tl (Some hd) results (hd :: acc)
       | Some '\'', 't' -> aux tl (Some hd) results (hd :: acc)
       | Some 't', '(' -> aux tl (Some hd) results (hd :: acc)
       | None, 'm' -> aux tl (Some hd) results (hd :: acc)
       | Some 'm', 'u' -> aux tl (Some hd) results (hd :: acc)
       | Some 'u', 'l' -> aux tl (Some hd) results (hd :: acc)
       | Some 'l', '(' -> aux tl (Some hd) results (hd :: acc)
       | Some '(', x when Char.is_digit x -> aux tl (Some hd) results (hd :: acc)
       | Some x, ',' when Char.is_digit x -> aux tl (Some hd) results (hd :: acc)
       | Some ',', x when Char.is_digit x -> aux tl (Some hd) results (hd :: acc)
       | Some x, y when Char.is_digit x && Char.is_digit y ->
         aux tl (Some hd) results (hd :: acc)
       | Some x, ')' when Char.is_digit x ->
         let instruction =
           List.rev (hd :: acc) |> String.of_list |> instruction_of_string
         in
         aux tl None (instruction :: results) []
       | _ -> aux tl None results [])
  in
  aux l None [] []
;;

let multiply_and_sum1 instructions =
  let rec aux instructions sum =
    match instructions with
    | [] -> sum
    | hd :: tl ->
      (match hd with
       | Do -> aux tl sum
       | Don't -> aux tl sum
       | Mul (a, b) -> aux tl (sum + (a * b)))
  in
  aux instructions 0
;;

let multiply_and_sum2 instructions =
  let rec aux instructions sum allow =
    match instructions with
    | [] -> sum
    | hd :: tl ->
      (match hd with
       | Do -> aux tl sum true
       | Don't -> aux tl sum false
       | Mul (a, b) when allow -> aux tl (sum + (a * b)) allow
       | Mul (_, _) -> aux tl sum allow)
  in
  aux instructions 0 true
;;

let () =
  let instructions = In_channel.read_all "day3/input.txt" |> parse in
  let sum1 = multiply_and_sum1 instructions in
  let sum2 = multiply_and_sum2 instructions in
  Out_channel.print_endline ("Part 1: " ^ Int.to_string sum1);
  Out_channel.print_endline ("Part 2: " ^ Int.to_string sum2)
;;
