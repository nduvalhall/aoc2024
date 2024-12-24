open Core

let input = In_channel.read_lines "day24/input.txt"

let wires =
  List.take_while input ~f:(fun s -> not (String.equal s ""))
  |> List.map ~f:(fun s ->
    let s =
      String.substr_replace_all s ~pattern:": " ~with_:":"
      |> String.split ~on:':'
    in
    List.nth_exn s 0, Int.of_string (List.nth_exn s 1))
;;

let gates =
  List.rev input
  |> List.take_while ~f:(fun s -> not (String.equal s ""))
  |> List.map ~f:(fun s ->
    let s =
      String.substr_replace_all s ~pattern:" -> " ~with_:":"
      |> String.split ~on:':'
    in
    let parents = List.nth_exn s 0 |> String.split ~on:' ' in
    let parents =
      (List.nth_exn parents 0, List.nth_exn parents 2), List.nth_exn parents 1
    in
    parents, List.nth_exn s 1)
;;

let rec solve gates w wires =
  match List.find wires ~f:(fun (w', _) -> String.equal w' w) with
  | None ->
    let ((p1, p2), op), _ =
      List.find gates ~f:(fun (_, w') -> String.equal w' w) |> Option.value_exn
    in
    let v1 =
      match List.find wires ~f:(fun (w', _) -> String.equal p1 w') with
      | None ->
        let _, w =
          List.find gates ~f:(fun (_, w') -> String.equal p1 w')
          |> Option.value_exn
        in
        solve gates w wires
      | Some (_, v) -> v
    in
    let v2 =
      match List.find wires ~f:(fun (w', _) -> String.equal p2 w') with
      | None ->
        let _, w =
          List.find gates ~f:(fun (_, w') -> String.equal p2 w')
          |> Option.value_exn
        in
        solve gates w wires
      | Some (_, v) -> v
    in
    (match op with
     | "AND" -> v1 land v2
     | "OR" -> v1 lor v2
     | "XOR" -> v1 lxor v2
     | _ -> failwith "Unknown operator")
  | Some (_, v) -> v
;;

let solve_all gates wires =
  List.fold gates ~init:wires ~f:(fun wires (_, w) ->
    (w, solve gates w wires) :: wires)
  |> List.stable_sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> List.filter ~f:(fun (w, _) -> String.is_prefix ~prefix:"z" w)
  |> List.rev
  |> List.fold ~init:0 ~f:(fun a (_, v) -> (a * 2) + v)
;;
