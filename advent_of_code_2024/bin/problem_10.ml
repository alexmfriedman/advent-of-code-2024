open! Core
open! Async

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

  let deltas = [ 0, 1; 0, -1; 1, 0; -1, 0 ]

  let steps { row; col } =
    List.map deltas ~f:(fun (row', col') -> { row = row + row'; col = col + col' })
  ;;
end

let num_trails ~grid =
  Memo.recursive ~hashable:Location.Table.hashable (fun num_trails loc ->
    match grid.(loc.Location.row).(loc.col) with
    | 9 -> 1
    | d ->
      let steps = Location.steps loc in
      List.sum
        (module Int)
        steps
        ~f:(fun loc' ->
          if
            0 <= loc'.Location.row
            && loc'.row < Array.length grid
            && 0 <= loc'.col
            && loc'.col < Array.length grid.(0)
            && grid.(loc'.row).(loc'.col) = d + 1
          then num_trails loc'
          else 0))
;;

let run ~filename =
  let%map file_contents = Reader.file_lines filename in
  let grid =
    List.map file_contents ~f:(fun columns ->
      String.to_array columns |> Array.map ~f:(fun c -> Char.to_string c |> Int.of_string))
    |> List.to_array
  in
  let num_trails = num_trails ~grid in
  let total = ref 0 in
  let grid' =
    Array.mapi grid ~f:(fun row grid_row ->
      Array.mapi grid_row ~f:(fun col d ->
        let num_trails = num_trails { Location.row; col } in
        (match d with
         | 0 -> total := !total + num_trails
         | _ -> ());
        num_trails))
  in
  print_s ([%sexp_of: int array array] grid');
  print_s ([%sexp_of: int] !total)
;;
