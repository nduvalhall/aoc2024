open Core

let print_string_set set =
  let elements = Set.to_list set in
  let elements_str = String.concat ~sep:", " elements in
  Printf.printf "{ %s }\n" elements_str
;;

let print_string_list l =
  let formatted = "[" ^ String.concat ~sep:", " l ^ "]" in
  Printf.printf "%s\n" formatted
;;

module Coord = struct
  type t = int * int

  let compare (a1, a2) (b1, b2) =
    match compare a1 b1 with
    | 0 -> compare a2 b2
    | c -> c
  ;;

  let sexp_of_t (a, b) =
    Sexp.List [ Sexp.Atom (Int.to_string a); Sexp.Atom (Int.to_string b) ]
  ;;

  let t_of_sexp sexp =
    match sexp with
    | Sexp.List [ Sexp.Atom a; Sexp.Atom b ] -> Int.of_string a, Int.of_string b
    | _ -> failwith "Invalid sexp for (int * int)"
  ;;
end

module CoordSet = Set.Make (Coord)
