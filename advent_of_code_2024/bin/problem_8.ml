open! Core
open! Async

module Antenna = struct
  let of_char = function
    | '.' -> None
    | c -> Some c
  ;;
end

module Location = struct
  module T = struct
    type t =
      { row : int
      ; col : int
      }
    [@@deriving hash, compare, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)
end

let rec all_pairs = function
  | [ _ ] | [] -> []
  | x :: xs -> List.map xs ~f:(fun x' -> x, x') @ all_pairs xs
;;

let rec add_while_on_board ~grid ~antinode_locations ~loc ~row' ~col' =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  if
    0 <= loc.Location.row
    && loc.Location.row < height
    && 0 <= loc.Location.col
    && loc.Location.col < width
  then (
    Hash_set.add antinode_locations loc;
    let loc' = { Location.row = loc.Location.row + row'; col = loc.col + col' } in
    add_while_on_board ~grid ~antinode_locations ~loc:loc' ~row' ~col')
;;

let antinode_locations ~grid =
  let antennas_to_locations = Char.Table.create () in
  let antinode_locations = Location.Hash_set.create () in
  Array.iteri grid ~f:(fun row grid_row ->
    Array.iteri grid_row ~f:(fun col -> function
      | None -> ()
      | Some char ->
        let location = { Location.row; col } in
        Hashtbl.update antennas_to_locations char ~f:(function
          | None -> [ location ]
          | Some l -> location :: l)));
  Hashtbl.iter antennas_to_locations ~f:(fun locations ->
    let all_location_pairs = all_pairs locations in
    List.iter all_location_pairs ~f:(fun (loc1, loc2) ->
      let row' = loc2.row - loc1.row in
      let col' = loc2.col - loc1.col in
      add_while_on_board ~grid ~antinode_locations ~loc:loc1 ~row':(-row') ~col':(-col');
      add_while_on_board ~grid ~antinode_locations ~loc:loc1 ~row' ~col'));
  antinode_locations
;;

let run ~filename =
  let%map file_contents = Reader.file_lines filename in
  let grid =
    List.map file_contents ~f:(fun columns ->
      String.to_array columns |> Array.map ~f:Antenna.of_char)
    |> List.to_array
  in
  let antinode_locations = antinode_locations ~grid in
  print_s ([%sexp_of: int] (Hash_set.length antinode_locations))
;;
