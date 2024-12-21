open! Core
open! Async

module Space = struct
  type t =
    | Empty
    | Obstacle

  let of_char = function
    | '.' | '^' -> Empty
    | '#' -> Obstacle
    | _ -> failwith "unreachable"
  ;;
end

module Dir = struct
  type t =
    | Up
    | Right
    | Down
    | Left

  let to_step = function
    | Right -> 0, 1
    | Up -> -1, 0
    | Down -> 1, 0
    | Left -> 0, -1
  ;;

  let turn_90 = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up
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

  let step { row; col } ~dir =
    let row', col' = Dir.to_step dir in
    { row = row + row'; col = col + col' }
  ;;
end

let rec step ~visited ~grid ~loc ~dir =
  Hash_set.add visited loc;
  let next_loc = Location.step loc ~dir in
  match grid.(next_loc.row).(next_loc.col) with
  | Space.Empty -> step ~visited ~grid ~loc:next_loc ~dir
  | Obstacle -> step ~visited ~grid ~loc ~dir:(Dir.turn_90 dir)
;;

let run ~filename =
  let%map file_contents = Reader.file_lines filename in
  let grid =
    List.map file_contents ~f:(fun columns ->
      String.to_array columns |> Array.map ~f:Space.of_char)
    |> List.to_array
  in
  let loc =
    List.find_mapi_exn file_contents ~f:(fun row s ->
      let%map.Option col, _ =
        String.findi s ~f:(fun _i -> function
          | '^' -> true
          | _ -> false)
      in
      { Location.row; col })
  in
  let visited = Location.Hash_set.create () in
  (try step ~visited ~grid ~loc ~dir:Up with
   | _ -> ());
  print_s ([%sexp_of: int] (Hash_set.length visited))
;;
