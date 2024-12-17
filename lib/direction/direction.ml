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
