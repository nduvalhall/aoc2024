open! Core

let towels, patterns =
  let lines = In_channel.read_lines "day19/input.txt" in
  let towels =
    List.nth_exn lines 0
    |> String.substr_replace_all ~pattern:", " ~with_:","
    |> String.split ~on:','
  in
  let patterns =
    List.rev lines |> List.take_while ~f:(fun s -> not (String.is_empty s))
  in
  towels, patterns
;;

let _print_set set =
  let elements = Set.to_list set in
  let elements_str = String.concat ~sep:", " elements in
  Printf.printf "{ %s }\n" elements_str
;;

let _print_list l =
  let formatted = "[" ^ String.concat ~sep:", " l ^ "]" in
  Printf.printf "%s\n" formatted
;;

let possible pattern towels =
  if String.is_empty pattern
  then true
  else (
    let rec aux prefixes visited =
      match prefixes with
      | [] -> false
      | prefix :: rest ->
        if String.equal prefix pattern
        then true
        else if (not (String.is_prefix ~prefix pattern))
                || Set.mem visited prefix
        then aux rest visited
        else (
          let visited = Set.add visited prefix in
          let new_prefixes = List.map towels ~f:(fun towel -> prefix ^ towel) in
          aux (new_prefixes @ rest) visited)
    in
    aux [ "" ] String.Set.empty)
;;

let () =
  let result = List.count patterns ~f:(fun p -> possible p towels) in
  Printf.printf "Number of matches: %d\n" result
;;
