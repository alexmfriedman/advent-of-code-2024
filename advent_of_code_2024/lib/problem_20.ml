open! Core
open! Async

module Space = struct
  type t =
    | Empty
    | Wall
end

module Grid = struct
  type t = Space.t array array
end

let shortest_path_with_possible_cheat ~(grid : Grid.t) ~start ~finish ~cheat =
  let visited = Grid_location.Hash_set.create () in
  let queue = Queue.create () in
  (* Apparently the start square counts as 1 ps *)
  Queue.enqueue queue (start, 1);
  (* NB: BFS seemed like the easiest implementation, even though there's only
     (up to) two paths and BFS is more general than necessary. *)
  let rec loop () =
    let grid_location, distance = Queue.dequeue_exn queue in
    if Grid_location.equal finish grid_location
    then distance
    else if Hash_set.mem visited grid_location
    then loop ()
    else (
      Hash_set.strict_add_exn visited grid_location;
      List.iter
        [ 0, 1; 0, -1; 1, 0; -1, 0 ]
        ~f:(fun (row', col') ->
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
  loop ()
;;

let count_tracks ~(grid : Grid.t) ~start ~finish =
  let baseline = shortest_path_with_possible_cheat ~grid ~start ~finish ~cheat:None in
  let num_tracks = ref 0 in
  Array.iteri grid ~f:(fun row grid_row ->
    Array.iteri grid_row ~f:(fun col space ->
      match space with
      | Space.Empty -> ()
      | Space.Wall ->
        let cheat = { Grid_location.row; col } in
        let shortest_path =
          shortest_path_with_possible_cheat ~grid ~start ~finish ~cheat:(Some cheat)
        in
        if baseline - shortest_path >= 100 then num_tracks := !num_tracks + 1));
  !num_tracks
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
