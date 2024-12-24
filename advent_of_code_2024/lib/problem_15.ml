open! Core
open! Async

module Move = struct
  type t =
    | Up
    | Right
    | Down
    | Left
  [@@deriving sexp_of]

  let of_char = function
    | '^' -> Up
    | '>' -> Right
    | 'v' -> Down
    | '<' -> Left
    | _ -> failwith "unreachable"
  ;;

  let to_delta = function
    | Up -> -1, 0
    | Right -> 0, 1
    | Down -> 1, 0
    | Left -> 0, -1
  ;;

  let affected_spaces t ~left_grid_location =
    let deltas =
      match t with
      | Left -> [ 0, -1 ]
      | Right -> [ 0, 2 ]
      | Up -> [ -1, 0; -1, 1 ]
      | Down -> [ 1, 0; 1, 1 ]
    in
    List.map deltas ~f:(fun (row', col') ->
      Grid_location.add_delta left_grid_location ~row' ~col')
  ;;
end

module At_least_one_wall_or_all_empty = struct
  type t =
    | At_least_one_wall
    | All_empty

  let combine t1 t2 =
    match t1, t2 with
    | At_least_one_wall, _ | _, At_least_one_wall -> At_least_one_wall
    | All_empty, All_empty -> All_empty
  ;;
end

module Box_side = struct
  type t =
    | Left
    | Right

  let to_char = function
    | Left -> '['
    | Right -> ']'
  ;;
end

module Space = struct
  type t =
    | No_wall of { maybe_box : Box_side.t option }
    | Wall

  let to_char = function
    | Wall -> '#'
    | No_wall { maybe_box = None } -> '.'
    | No_wall { maybe_box = Some box_side } -> Box_side.to_char box_side
  ;;
end

module Game_state = struct
  type t =
    { grid : Space.t array array
    ; mutable robot_grid_location : Grid_location.t
    }

  let _print_grid t =
    Array.iteri t.grid ~f:(fun row grid_row ->
      Array.iteri grid_row ~f:(fun col space ->
        let char =
          if Grid_location.equal t.robot_grid_location { Grid_location.row; col }
          then '@'
          else Space.to_char space
        in
        print_char char);
      print_endline "")
  ;;

  let of_array char_array =
    let robot_grid_location = Set_once.create () in
    let grid =
      Array.mapi char_array ~f:(fun row char_row ->
        Array.concat_mapi char_row ~f:(fun col char ->
          match char with
          | '@' ->
            Set_once.set_exn
              robot_grid_location
              [%here]
              { Grid_location.row; col = col * 2 };
            [| Space.No_wall { maybe_box = None }; Space.No_wall { maybe_box = None } |]
          | '.' ->
            [| Space.No_wall { maybe_box = None }; Space.No_wall { maybe_box = None } |]
          | 'O' ->
            [| Space.No_wall { maybe_box = Some Left }
             ; Space.No_wall { maybe_box = Some Right }
            |]
          | '#' -> [| Wall; Wall |]
          | _ -> failwith "unreachable"))
    in
    { grid; robot_grid_location = Set_once.get_exn robot_grid_location [%here] }
  ;;

  let rec get_adjacent_box_set_with_stop t ~box_set ~grid_location ~move
    : At_least_one_wall_or_all_empty.t
    =
    if Hashtbl.mem box_set grid_location
    then At_least_one_wall_or_all_empty.All_empty
    else (
      (* No need to do bounds checks since the outside is guaranteed to have a wall *)
      match t.grid.(grid_location.Grid_location.row).(grid_location.col) with
      | No_wall { maybe_box = None } -> At_least_one_wall_or_all_empty.All_empty
      | Wall -> At_least_one_wall
      | No_wall { maybe_box = Some box_side } ->
        let left_grid_location =
          match box_side with
          | Left -> grid_location
          | Right ->
            { Grid_location.row = grid_location.row; col = grid_location.col - 1 }
        in
        Hashtbl.add_exn box_set ~key:left_grid_location ~data:Box_side.Left;
        Hashtbl.add_exn
          box_set
          ~key:
            { Grid_location.row = left_grid_location.row
            ; col = left_grid_location.col + 1
            }
          ~data:Right;
        let affected_spaces = Move.affected_spaces move ~left_grid_location in
        List.fold
          affected_spaces
          ~init:At_least_one_wall_or_all_empty.All_empty
          ~f:(fun acc grid_location ->
            let stop = get_adjacent_box_set_with_stop t ~box_set ~grid_location ~move in
            At_least_one_wall_or_all_empty.combine acc stop))
  ;;

  let move t ~move =
    let row', col' = Move.to_delta move in
    let robot_grid_location = t.robot_grid_location in
    let box_set = Grid_location.Table.create () in
    let stop =
      get_adjacent_box_set_with_stop
        t
        ~box_set
        ~grid_location:(Grid_location.add_delta robot_grid_location ~row' ~col')
        ~move
    in
    match stop with
    | At_least_one_wall -> ()
    | All_empty ->
      let robot_grid_location' =
        Grid_location.add_delta t.robot_grid_location ~row' ~col'
      in
      Hashtbl.iter_keys box_set ~f:(fun { Grid_location.row; col } ->
        t.grid.(row).(col) <- No_wall { maybe_box = None });
      Hashtbl.iteri box_set ~f:(fun ~key:grid_location ~data:box_side ->
        let { Grid_location.row; col } =
          Grid_location.add_delta grid_location ~row' ~col'
        in
        t.grid.(row).(col) <- No_wall { maybe_box = Some box_side });
      t.robot_grid_location <- robot_grid_location'
  ;;

  let sum_gps_coordinate_score t =
    let sum = ref 0 in
    Array.iteri t.grid ~f:(fun row grid_row ->
      Array.iteri grid_row ~f:(fun col -> function
        | No_wall { maybe_box = Some Left } -> sum := !sum + ((100 * row) + col)
        | No_wall { maybe_box = Some Right | None } | Wall -> ()));
    !sum
  ;;
end

let run ~filename =
  let%map file_contents = Reader.file_lines filename in
  let split_idx, _empty_string =
    List.findi_exn file_contents ~f:(fun _idx -> String.is_empty)
  in
  let grid_string, moves_string = List.split_n file_contents split_idx in
  let game_state =
    List.to_array grid_string |> Array.map ~f:String.to_array |> Game_state.of_array
  in
  let moves =
    String.concat moves_string ~sep:"" |> String.to_array |> Array.map ~f:Move.of_char
  in
  (* print_endline "Initial state";
  Game_state.print_grid game_state; *)
  Array.iter moves ~f:(fun move ->
    Game_state.move game_state ~move
    (* ; printf !"\nMove: %{sexp: Move.t}\n" move;
    Game_state.print_grid game_state *));
  let sum_gps_coordinate_score = Game_state.sum_gps_coordinate_score game_state in
  print_s ([%sexp_of: int] sum_gps_coordinate_score)
;;
