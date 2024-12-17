open! Core

module type ELEMENT = sig
  type t
end

module Grid (Element : ELEMENT) = struct
  type t = Element.t array array

  (** Create empty grid with width and height, fill with default value*)
  let empty ~width ~height ~default : t =
    Array.make_matrix ~dimx:width ~dimy:height default
  ;;

  (** Get dimensions of grid, returns width and height*)
  let dimensions (grid : t) =
    let h = Array.length grid in
    let w = if h = 0 then 0 else Array.length grid.(0) in
    w, h
  ;;

  (** Checks if position is valid*)
  let within_bounds (grid : t) (x, y) =
    let w, h = dimensions grid in
    x >= 0 && x < w && y >= 0 && y < h
  ;;
end
