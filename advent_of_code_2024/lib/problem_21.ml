open! Core
open! Async

let num_robots = 2

module Move = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving equal, compare, hash, sexp_of, enumerate]
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
  [@@deriving equal, compare, hash, sexp_of, enumerate]

  let to_char = function
    | A -> 'A'
    | Num_0 -> '0'
    | Num_1 -> '1'
    | Num_2 -> '2'
    | Num_3 -> '3'
    | Num_4 -> '4'
    | Num_5 -> '5'
    | Num_6 -> '6'
    | Num_7 -> '7'
    | Num_8 -> '8'
    | Num_9 -> '9'
  ;;

  let _of_char = function
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
end

module State = struct
  module T = struct
    type t =
      { robot_numeric_keypad : Numeric_keypad.t
      ; (* Apparently you can't hash arrays *)
        robot_directional_keypads : Directional_keypad.t List.t
      ; num_correct_numeric_keypad_entries : int
      }
    [@@deriving equal, compare, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)

  let empty =
    { robot_numeric_keypad = Numeric_keypad.A
    ; robot_directional_keypads = List.init num_robots ~f:(Fn.const Directional_keypad.A)
    ; num_correct_numeric_keypad_entries = 0
    }
  ;;

  let push_button t ~target ~(next_human_button_press : Directional_keypad.t) : t option =
    let open Option.Let_syntax in
    let rec apply_at_position (next_button_press : Directional_keypad.t) = function
      | [] ->
        (match next_button_press with
         | Move move ->
           let%map robot_numeric_keypad' =
             Numeric_keypad.apply_move_to_robot_hand t.robot_numeric_keypad ~move
           in
           robot_numeric_keypad', [], t.num_correct_numeric_keypad_entries
         | A ->
           let char_pressed = Numeric_keypad.to_char t.robot_numeric_keypad in
           if
             Char.equal
               (String.get target t.num_correct_numeric_keypad_entries)
               char_pressed
           then Some (t.robot_numeric_keypad, [], t.num_correct_numeric_keypad_entries + 1)
           else None)
      | robot_directional_keypad :: robot_directional_keypads ->
        (match next_button_press with
         | Directional_keypad.Move move ->
           let%map robot_directional_keypad' =
             Directional_keypad.apply_move_to_robot_hand robot_directional_keypad ~move
           in
           ( t.robot_numeric_keypad
           , robot_directional_keypad' :: robot_directional_keypads
           , t.num_correct_numeric_keypad_entries )
         | A ->
           let%map
               ( robot_numeric_keypad
               , robot_directional_keypads
               , num_correct_numeric_keypad_entries )
             =
             apply_at_position robot_directional_keypad robot_directional_keypads
           in
           ( robot_numeric_keypad
           , robot_directional_keypad :: robot_directional_keypads
           , num_correct_numeric_keypad_entries ))
    in
    let%map
        ( robot_numeric_keypad
        , robot_directional_keypads
        , num_correct_numeric_keypad_entries )
      =
      apply_at_position next_human_button_press t.robot_directional_keypads
    in
    { robot_numeric_keypad
    ; robot_directional_keypads
    ; num_correct_numeric_keypad_entries
    }
  ;;
end

let bfs_state_space target =
  let queue = Queue.create () in
  Queue.enqueue queue (State.empty, 0);
  let visited = State.Hash_set.create () in
  let rec loop () =
    let state, num_presses = Queue.dequeue_exn queue in
    if state.num_correct_numeric_keypad_entries = 4
    then num_presses
    else if Hash_set.mem visited state
    then loop ()
    else (
      Hash_set.strict_add_exn visited state;
      List.iter Directional_keypad.all ~f:(fun next_human_button_press ->
        State.push_button state ~target ~next_human_button_press
        |> Option.iter ~f:(fun state' ->
          if not (Hash_set.mem visited state')
          then Queue.enqueue queue (state', num_presses + 1)));
      loop ())
  in
  loop ()
;;

let run ~filename =
  let%map codes = Reader.file_lines filename in
  let num_presses =
    List.sum
      (module Int)
      codes
      ~f:(fun target ->
        let num_presses = bfs_state_space target in
        let numeric_code = String.drop_suffix target 1 |> Int.of_string in
        let result = num_presses * numeric_code in
        Core.print_s ([%sexp_of: int * int * int] (num_presses, numeric_code, result));
        result)
  in
  print_s ([%sexp_of: int] num_presses)
;;
