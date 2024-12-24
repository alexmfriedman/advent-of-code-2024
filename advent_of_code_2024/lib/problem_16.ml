open! Core
open! Async

module Direction = struct
  type t =
    | Up
    | Right
    | Down
    | Left
  [@@deriving sexp_of, hash, compare]

  let to_delta = function
    | Up -> -1, 0
    | Right -> 0, 1
    | Down -> 1, 0
    | Left -> 0, -1
  ;;

  let rotate_clockwise = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up
  ;;

  let rotate_counterclockwise = function
    | Right -> Up
    | Down -> Right
    | Left -> Down
    | Up -> Left
  ;;
end

module Space = struct
  type t =
    | Empty
    | Wall
    | Finish
end

module Grid = struct
  type t = Space.t array array

  let of_array char_array =
    let start_location = Set_once.create () in
    let grid =
      Array.mapi char_array ~f:(fun row char_row ->
        Array.mapi char_row ~f:(fun col char ->
          match char with
          | 'S' ->
            Set_once.set_exn start_location [%here] { Grid_location.row; col };
            Space.Empty
          | '.' -> Empty
          | 'E' -> Finish
          | '#' -> Wall
          | _ -> failwith "unreachable"))
    in
    grid, Set_once.get_exn start_location [%here]
  ;;
end

module Position = struct
  module T = struct
    type t =
      { grid_location : Grid_location.t
      ; direction : Direction.t
      }
    [@@deriving sexp_of, hash, compare]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)

  let move_in_current_direction { grid_location; direction } =
    let row', col' = Direction.to_delta direction in
    { grid_location = Grid_location.add_delta grid_location ~row' ~col'; direction }
  ;;

  let rotate_clockwise { grid_location; direction } =
    { grid_location; direction = Direction.rotate_clockwise direction }
  ;;

  let rotate_counterclockwise { grid_location; direction } =
    { grid_location; direction = Direction.rotate_counterclockwise direction }
  ;;
end

module Position_heap = struct
  (* Possible positions, by cost *)
  type t = Grid_location.Set.t Position.Map.t Int.Map.t

  let create ~initial_position : t =
    Int.Map.singleton
      0
      (Position.Map.singleton
         initial_position
         (Grid_location.Set.singleton initial_position.grid_location))
  ;;

  let pop_all_mins (t : t) : int * Grid_location.Set.t Position.Map.t * t =
    let cost, positions = Map.min_elt_exn t in
    let t' = Map.remove t cost in
    cost, positions, t'
  ;;

  let push (t : t) ~cost ~position ~visited_on_path : t =
    Map.update t cost ~f:(fun maybe_positions ->
      let positions = Option.value maybe_positions ~default:Position.Map.empty in
      Map.update positions position ~f:(fun maybe_grid_locations ->
        let grid_locations =
          Option.value maybe_grid_locations ~default:Grid_location.Set.empty
        in
        Set.union grid_locations visited_on_path))
  ;;
end

let dijkstra ~(grid : Grid.t) ~(initial_position : Position.t) =
  let visited_set = Position.Hash_set.create () in
  let heap = ref (Position_heap.create ~initial_position) in
  let rec loop () =
    let cost, positions, heap' = Position_heap.pop_all_mins !heap in
    heap := heap';
    let solutions =
      Map.to_sequence positions
      |> Sequence.filter_map ~f:(fun (position, visited_on_path) ->
        if Hash_set.mem visited_set position
        then None
        else (
          Hash_set.add visited_set position;
          match grid.(position.grid_location.row).(position.grid_location.col) with
          | Space.Finish -> Some visited_on_path
          | Wall -> None
          | Empty ->
            let positions_with_cost =
              [ Position.move_in_current_direction position, 1
              ; Position.rotate_clockwise position, 1000
              ; Position.rotate_counterclockwise position, 1000
              ]
            in
            List.iter positions_with_cost ~f:(fun (position, cost') ->
              let visited_on_path =
                Set.add visited_on_path position.Position.grid_location
              in
              heap
              := Position_heap.push !heap ~cost:(cost + cost') ~position ~visited_on_path);
            None))
      |> Sequence.to_list
    in
    match solutions with
    | [] -> loop ()
    | _ -> solutions
  in
  loop () |> Grid_location.Set.union_list |> Set.length
;;

let run ~filename =
  let%map file_contents = Reader.file_lines filename in
  let grid, grid_location =
    List.to_array file_contents |> Array.map ~f:String.to_array |> Grid.of_array
  in
  let num_distinct_elements_on_path =
    dijkstra ~grid ~initial_position:{ Position.grid_location; direction = Right }
  in
  print_s ([%sexp_of: int] num_distinct_elements_on_path)
;;
