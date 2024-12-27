open! Core
open! Async

let deltas = [ 0, 1; 0, -1; 1, 0; -1, 0 ]

module Space = struct
  type t =
    | Empty
    | Wall
end

module Grid = struct
  type t = Space.t array array

  let all_grid_locations (t : t) =
    let rows = List.init (Array.length t) ~f:Fn.id in
    let cols = List.init (Array.length t.(0)) ~f:Fn.id in
    List.cartesian_product rows cols
    |> List.map ~f:(fun (row, col) -> { Grid_location.row; col })
  ;;
end

let calc_distance_from_start_and_finish ~(grid : Grid.t) ~start ~finish ~cheat =
  let distance_from_start = Grid_location.Table.create () in
  let queue = Queue.create () in
  Queue.enqueue queue (start, 0);
  (* NB: BFS seemed like the easiest implementation, even though there's only
     (up to) two paths and BFS is more general than necessary. *)
  let rec loop () =
    let grid_location, distance = Queue.dequeue_exn queue in
    if Grid_location.equal finish grid_location
    then distance
    else if Hashtbl.mem distance_from_start grid_location
    then loop ()
    else (
      Hashtbl.add_exn distance_from_start ~key:grid_location ~data:distance;
      List.iter deltas ~f:(fun (row', col') ->
        let grid_location' = Grid_location.add_delta grid_location ~row' ~col' in
        let is_effectively_empty =
          try
            match grid.(grid_location'.row).(grid_location'.col) with
            | Space.Wall ->
              Option.exists cheat ~f:(fun cheat ->
                Grid_location.equal cheat grid_location')
            | Empty -> true
          with
          | _ -> false
        in
        if is_effectively_empty then Queue.enqueue queue (grid_location', distance + 1));
      loop ())
  in
  let total_distance = loop () in
  Hashtbl.add_exn distance_from_start ~key:finish ~data:total_distance;
  let distance_from_finish =
    Hashtbl.map distance_from_start ~f:(fun distance_from_start ->
      total_distance - distance_from_start)
  in
  total_distance, distance_from_start, distance_from_finish
;;

let distance_between_grid_locations
      (grid_location_1 : Grid_location.t)
      (grid_location_2 : Grid_location.t)
  =
  Int.abs (grid_location_1.row - grid_location_2.row)
  + Int.abs (grid_location_1.col - grid_location_2.col)
;;

let distance_of_cheat_path
      ~distance_from_start
      ~distance_from_finish
      ~cheat_start
      ~cheat_finish
  =
  let open Option.Let_syntax in
  let distance_of_cheat = distance_between_grid_locations cheat_start cheat_finish in
  let%bind () = Option.some_if (distance_of_cheat <= 20) () in
  let%bind cheat_start_distance_from_start =
    Hashtbl.find distance_from_start cheat_start
  in
  let%map cheat_finish_distance_from_finish =
    Hashtbl.find distance_from_finish cheat_finish
  in
  cheat_start_distance_from_start + distance_of_cheat + cheat_finish_distance_from_finish
;;

let count_tracks ~(grid : Grid.t) ~start ~finish =
  let total_distance, distance_from_start, distance_from_finish =
    calc_distance_from_start_and_finish ~grid ~start ~finish ~cheat:None
  in
  let all_grid_locations = Grid.all_grid_locations grid in
  List.cartesian_product all_grid_locations all_grid_locations
  |> List.sum
       (module Int)
       ~f:(fun (cheat_start, cheat_finish) ->
         match
           distance_of_cheat_path
             ~distance_from_start
             ~distance_from_finish
             ~cheat_start
             ~cheat_finish
         with
         | Some distance when total_distance - distance >= 100 -> 1
         | _ -> 0)
;;

let run ~filename =
  let%map file_lines = Reader.file_lines filename in
  let start = Set_once.create () in
  let finish = Set_once.create () in
  let grid =
    List.to_array file_lines
    |> Array.mapi ~f:(fun row s ->
      String.to_array s
      |> Array.mapi ~f:(fun col c ->
        match c with
        | '#' -> Space.Wall
        | '.' -> Empty
        | 'S' ->
          Set_once.set_exn start [%here] { Grid_location.row; col };
          Empty
        | 'E' ->
          Set_once.set_exn finish [%here] { Grid_location.row; col };
          Empty
        | _ -> failwithf !"unreachable: %s" (Char.to_string c) ()))
  in
  let num_tracks =
    count_tracks
      ~grid
      ~start:(Set_once.get_exn start [%here])
      ~finish:(Set_once.get_exn finish [%here])
  in
  print_s ([%sexp_of: int] num_tracks)
;;
