open Core

type node = int * int

module Graph = Map.Make (struct
    type t = node

    let compare (a1, a2) (b1, b2) =
      match Int.compare a1 b1 with
      | 0 -> compare a2 b2
      | c -> c
    ;;

    let sexp_of_t (a, b) =
      Sexp.List [ Sexp.Atom (Int.to_string a); Sexp.Atom (Int.to_string b) ]
    ;;

    let t_of_sexp sexp =
      match sexp with
      | Sexp.List [ Sexp.Atom a; Sexp.Atom b ] ->
        Int.of_string a, Int.of_string b
      | _ -> failwith "Invalid sexp for (int * int)"
    ;;
  end)

include Map
include Graph

let add_edge graph node neighbour =
  let data =
    match Map.find graph node with
    | Some ns -> neighbour :: ns
    | None -> [ neighbour ]
  in
  Map.set graph ~key:node ~data
;;

let of_grid grid =
  let rows = Array.length grid in
  let cols = if rows = 0 then 0 else Array.length grid.(0) in
  let is_valid (i, j) = i >= 0 && i < rows && j >= 0 && j < cols in
  let rec build_graph grid i j graph =
    if i >= rows
    then graph
    else (
      let neighbours = [ i - 1, j; i + 1, j; i, j - 1; i, j + 1 ] in
      let graph_with_edges =
        List.fold neighbours ~init:graph ~f:(fun acc neighbour ->
          if is_valid neighbour then add_edge acc (i, j) neighbour else acc)
      in
      if j + 1 < cols
      then build_graph grid i (j + 1) graph_with_edges
      else build_graph grid (i + 1) 0 graph_with_edges)
  in
  build_graph grid 0 0 Graph.empty
;;

let length graph = Map.length graph

let print graph =
  Map.iteri graph ~f:(fun ~key:(i, j) ~data:neighbours ->
    Printf.printf "(%d, %d) -> " i j;
    List.iter neighbours ~f:(fun (i, j) -> Printf.printf "(%d, %d)  " i j);
    print_endline "")
;;

let find node = Map.find node
