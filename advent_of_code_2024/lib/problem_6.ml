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

let rec has_loop ~n ~visited ~grid ~loc ~dir =
  (* There are this many spaces, each with 4 directions, so if we've traveled
  more than this many positions then PHP says we must be in a position we've
  aleady visited *)
  if n > Array.length grid * Array.length grid.(0) * 4
  then true
  else (
    Hash_set.add visited loc;
    let next_loc = Location.step loc ~dir in
    match grid.(next_loc.row).(next_loc.col) with
    | Space.Empty -> has_loop ~n:(n + 1) ~visited ~grid ~loc:next_loc ~dir
    | Obstacle -> has_loop ~n:(n + 1) ~visited ~grid ~loc ~dir:(Dir.turn_90 dir))
;;

let has_loop ~grid ~loc =
  let visited = Location.Hash_set.create () in
  try has_loop ~n:0 ~visited ~grid ~loc ~dir:Up with
  | _ -> false
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
  let obstacle_count = ref 0 in
  Array.iteri grid ~f:(fun row spaces ->
    Array.iteri spaces ~f:(fun col space ->
      match space with
      | Obstacle -> ()
      | Empty ->
        grid.(row).(col) <- Obstacle;
        if has_loop ~grid ~loc then obstacle_count := !obstacle_count + 1;
        grid.(row).(col) <- Empty));
  print_s ([%sexp_of: int] !obstacle_count)
;;
