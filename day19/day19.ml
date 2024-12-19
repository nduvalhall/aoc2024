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

module Key = struct
  type t = string * string list

  let compare (s1, l1) (s2, l2) =
    let cmp = String.compare s1 s2 in
    if cmp <> 0 then cmp else List.compare String.compare l1 l2
  ;;

  let hash (s, l) = Hashtbl.hash (s, List.map ~f:Hashtbl.hash l)

  let hash_fold_t state (s, l) =
    let state = String.hash_fold_t state s in
    List.fold l ~init:state ~f:String.hash_fold_t
  ;;

  let sexp_of_t (s, l) =
    Sexp.List [ Sexp.Atom s; Sexp.List (List.map l ~f:(fun s -> Sexp.Atom s)) ]
  ;;
end

let memoize f =
  let table =
    Hashtbl.create
      (module struct
        include Key

        let t_of_sexp _ = failwith "Not implemented"
      end)
  in
  fun arg1 arg2 ->
    Hashtbl.find_or_add table (arg1, arg2) ~default:(fun () -> f arg1 arg2)
;;

let memoized_count_matches =
  let rec count_matches current remaining target =
    printf "current %s, target %s\n" current target;
    match remaining with
    | [] -> if String.equal current target then 1 else 0
    | _ when String.equal current target -> 1
    | _ when String.length current > String.length target -> 0
    | _ when not (String.is_prefix target ~prefix:current) -> 0
    | _ ->
      List.fold_left
        ~init:0
        ~f:(fun acc (i, char) ->
          let new_remaining =
            List.mapi ~f:(fun j x -> if i = j then None else Some x) remaining
            |> List.filter_map ~f:(fun x -> x)
          in
          acc + memoized_count_matches (current ^ char) new_remaining target)
        (List.mapi ~f:(fun i x -> i, x) remaining)
  and memoized = memoize count_matches in
  memized
;;

let () =
  let result =
    List.fold patterns ~init:0 ~f:(fun acc pattern ->
      memoized_count_matches "" towels pattern + acc)
  in
  Printf.printf "Number of matches: %d\n" result
;;
