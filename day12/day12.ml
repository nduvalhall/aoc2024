open Core

module Grid = struct
  type t =
    { g : char array array
    ; w : int
    ; h : int
    }

  let get grid (x, y) =
    if x >= 0 && x < grid.w && y >= 0 && y < grid.w
    then Some grid.g.(y).(x)
    else None
  ;;

  let from_input input =
    let g =
      In_channel.read_lines input
      |> List.to_array
      |> Array.map ~f:String.to_array
    in
    let w = Array.length g.(0)
    and h = Array.length g in
    { g; w; h }
  ;;

  let print grid =
    Array.iter grid.g ~f:(fun a ->
      Array.iter a ~f:(fun c -> Printf.printf "%c " c);
      Printf.printf "\n")
  ;;
end

let grid = Grid.from_input "day12/test.txt"
