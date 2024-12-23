type t =
  | Up
  | Right
  | Down
  | Left

let equal a b =
  match a, b with
  | Up, Up -> true
  | Right, Right -> true
  | Down, Down -> true
  | Left, Left -> true
  | _ -> false
;;

let diff (ax, ay) (bx, by) =
  match bx - ax, by - ay with
  | -1, 0 -> Up
  | 0, 1 -> Right
  | 1, 0 -> Down
  | 0, -1 -> Left
  | _ ->
    failwith
      (Printf.sprintf "No direction found for (%d, %d) -> (%d, %d)" ax ay bx by)
;;

let clockwise d =
  match d with
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up
;;

let anticlockwise d =
  match d with
  | Up -> Left
  | Left -> Down
  | Down -> Right
  | Right -> Up
;;

let flip d =
  match d with
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left
;;

let translate d (x, y) =
  match d with
  | Up -> x - 1, y
  | Right -> x, y + 1
  | Down -> x + 1, y
  | Left -> x, y - 1
;;

let as_list = [ Up; Right; Down; Left ]
