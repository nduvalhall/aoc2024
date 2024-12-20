open Core

let make_lists lines =
  let rec aux lines list1 list2 =
    match lines with
    | [] -> list1, list2
    | head :: tail ->
      let split =
        String.split ~on:' ' head |> List.filter ~f:(String.( <> ) "")
      in
      aux
        tail
        (Int.of_string (List.nth_exn split 0) :: list1)
        (Int.of_string (List.nth_exn split 1) :: list2)
  in
  aux lines [] []
;;

let sort_lists lists =
  match lists with
  | a, b -> List.sort a ~compare, List.sort b ~compare
;;

let calculate_distance lists =
  let rec aux lists sum =
    match lists with
    | [], _ | _, [] -> sum
    | head1 :: tail1, head2 :: tail2 ->
      aux (tail1, tail2) (Int.abs (head1 - head2) + sum)
  in
  aux lists 0
;;

let calculate_similarity_score lists =
  let rec aux lists sum =
    match lists with
    | [], _ | _, [] -> sum
    | head :: tail, other ->
      let score = List.count other ~f:(Int.equal head) in
      aux (tail, other) ((head * score) + sum)
  in
  aux lists 0
;;

let () =
  let lists =
    In_channel.read_lines "day1/input.txt" |> make_lists |> sort_lists
  in
  let part1 = calculate_distance lists in
  let part2 = calculate_similarity_score lists in
  Out_channel.print_endline ("Part 1: " ^ Int.to_string part1);
  Out_channel.print_endline ("Part 2: " ^ Int.to_string part2)
;;
