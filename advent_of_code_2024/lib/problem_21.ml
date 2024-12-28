open! Core
open! Async

let num_directional_robots = 25

module Move = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving equal, compare, hash, sexp_of, enumerate, variants]
end

module Numeric_keypad = struct
  type t =
    | A
    | Num_0
    | Num_1
    | Num_2
    | Num_3
    | Num_4
    | Num_5
    | Num_6
    | Num_7
    | Num_8
    | Num_9
  [@@deriving equal, compare, hash, sexp_of, enumerate, variants]

  let of_char = function
    | 'A' -> A
    | '0' -> Num_0
    | '1' -> Num_1
    | '2' -> Num_2
    | '3' -> Num_3
    | '4' -> Num_4
    | '5' -> Num_5
    | '6' -> Num_6
    | '7' -> Num_7
    | '8' -> Num_8
    | '9' -> Num_9
    | _ -> failwith "unreachable"
  ;;

  let to_grid_location t =
    let row, col =
      match t with
      | A -> 3, 2
      | Num_0 -> 3, 1
      | Num_1 -> 2, 0
      | Num_2 -> 2, 1
      | Num_3 -> 2, 2
      | Num_4 -> 1, 0
      | Num_5 -> 1, 1
      | Num_6 -> 1, 2
      | Num_7 -> 0, 0
      | Num_8 -> 0, 1
      | Num_9 -> 0, 2
    in
    { Grid_location.row; col }
  ;;

  let of_grid_location =
    let grid_locations_to_t =
      Grid_location.Map.of_list_with_key_exn all ~get_key:to_grid_location
    in
    Map.find grid_locations_to_t
  ;;

  let apply_move_to_robot_hand t ~(move : Move.t) =
    let row', col' =
      match move with
      | Up -> -1, 0
      | Down -> 1, 0
      | Left -> 0, -1
      | Right -> 0, 1
    in
    Grid_location.add_delta (to_grid_location t) ~row' ~col' |> of_grid_location
  ;;

  let optimal_moves_from_prev ~prev t =
    let { Grid_location.row; col } = to_grid_location t in
    let { Grid_location.row = prev_row; col = prev_col } = to_grid_location prev in
    let row', col' = row - prev_row, col - prev_col in
    let vertical = if row' < 0 then Move.Up else Down in
    let vertical = List.init (Int.abs row') ~f:(Fn.const vertical) in
    let horizontal = if col' > 0 then Move.Right else Left in
    let horizontal = List.init (Int.abs col') ~f:(Fn.const horizontal) in
    [ vertical @ horizontal; horizontal @ vertical ]
    |> List.filter ~f:(fun moves ->
      List.fold_result moves ~init:prev ~f:(fun prev move ->
        let next = apply_move_to_robot_hand prev ~move in
        Result.of_option next ~error:())
      |> Result.is_ok)
  ;;
end

module Directional_keypad = struct
  type t =
    | Move of Move.t
    | A
  [@@deriving equal, compare, hash, sexp_of, enumerate]

  let to_grid_location t =
    let row, col =
      match t with
      | A -> 0, 2
      | Move Up -> 0, 1
      | Move Left -> 1, 0
      | Move Down -> 1, 1
      | Move Right -> 1, 2
    in
    { Grid_location.row; col }
  ;;

  let of_grid_location =
    let grid_locations_to_t =
      Grid_location.Map.of_list_with_key_exn all ~get_key:to_grid_location
    in
    Map.find grid_locations_to_t
  ;;

  let apply_move_to_robot_hand t ~(move : Move.t) =
    let row', col' =
      match move with
      | Up -> -1, 0
      | Down -> 1, 0
      | Left -> 0, -1
      | Right -> 0, 1
    in
    Grid_location.add_delta (to_grid_location t) ~row' ~col' |> of_grid_location
  ;;

  let optimal_moves_from_prev ~prev t =
    let { Grid_location.row; col } = to_grid_location t in
    let { Grid_location.row = prev_row; col = prev_col } = to_grid_location prev in
    let row', col' = row - prev_row, col - prev_col in
    let vertical = if row' < 0 then Move.Up else Down in
    let vertical = List.init (Int.abs row') ~f:(Fn.const vertical) in
    let horizontal = if col' > 0 then Move.Right else Left in
    let horizontal = List.init (Int.abs col') ~f:(Fn.const horizontal) in
    [ vertical @ horizontal; horizontal @ vertical ]
    |> List.filter ~f:(fun moves ->
      List.fold_result moves ~init:prev ~f:(fun prev move ->
        let next = apply_move_to_robot_hand prev ~move in
        Result.of_option next ~error:())
      |> Result.is_ok)
  ;;
end

module Dp_input = struct
  module T = struct
    type t =
      { moves : Move.t list
      ; num_robot_levels_remaining : int
      }
    [@@deriving equal, compare, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)
end

let num_presses_to_move_at_level_and_reset_to_a =
  Memo.recursive
    ~hashable:Dp_input.hashable
    (fun
        num_presses_to_move_at_level_and_reset_to_a
         { moves; num_robot_levels_remaining }
       ->
       match Int.compare num_robot_levels_remaining 0 |> Ordering.of_int with
       | Less -> failwith "unreachable"
       | Equal ->
         List.length moves
         +
         (* Press a *)
         1
       | Greater ->
         let prev, min_moves =
           List.fold
             moves
             ~init:(Directional_keypad.A, 0)
             ~f:(fun (prev, total_moves) curr ->
               let curr = Directional_keypad.Move curr in
               if Directional_keypad.equal prev curr
               then
                 (* If we're already at the button we need to press, just push A *)
                 curr, 1 + total_moves
               else (
                 let num_moves =
                   Directional_keypad.optimal_moves_from_prev ~prev curr
                   |> List.map ~f:(fun moves ->
                     num_presses_to_move_at_level_and_reset_to_a
                       { Dp_input.moves
                       ; num_robot_levels_remaining = num_robot_levels_remaining - 1
                       })
                   |> List.min_elt ~compare:Int.compare
                   |> Option.value_exn
                 in
                 curr, num_moves + total_moves))
         in
         let to_reset =
           let num_moves =
             Directional_keypad.optimal_moves_from_prev ~prev A
             |> List.map ~f:(fun moves ->
               num_presses_to_move_at_level_and_reset_to_a
                 { Dp_input.moves
                 ; num_robot_levels_remaining = num_robot_levels_remaining - 1
                 })
             |> List.min_elt ~compare:Int.compare
             |> Option.value_exn
           in
           num_moves
         in
         min_moves + to_reset)
;;

let run ~filename =
  let%map codes = Reader.file_lines filename in
  let num_presses =
    List.sum
      (module Int)
      codes
      ~f:(fun target ->
        let sum = ref 0 in
        String.iteri target ~f:(fun i c ->
          let prev =
            if i = 0
            then Numeric_keypad.A
            else String.get target (i - 1) |> Numeric_keypad.of_char
          in
          let optimal_count =
            Numeric_keypad.optimal_moves_from_prev ~prev (Numeric_keypad.of_char c)
            |> List.map ~f:(fun moves ->
              num_presses_to_move_at_level_and_reset_to_a
                { Dp_input.moves; num_robot_levels_remaining = num_directional_robots })
            |> List.min_elt ~compare:Int.compare
            |> Option.value_exn
          in
          let prefix = String.drop_suffix target 1 |> Int.of_string in
          sum := !sum + (optimal_count * prefix));
        !sum)
  in
  print_s ([%sexp_of: int] num_presses)
;;
