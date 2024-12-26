open! Core
open! Async

let grid_height = 71
let grid_width = 71

module Space = struct
  type t =
    | Empty
    | Filled
end

module Grid = struct
  type t = Space.t array array

  let create () : t =
    Array.init grid_height ~f:(fun _ -> Array.create ~len:grid_width Space.Empty)
  ;;
end

let bfs grid ~(start_location : Grid_location.t) ~(finish_location : Grid_location.t) =
  let queue = Queue.create () in
  Queue.enqueue queue (start_location, 0);
  let visited = Grid_location.Hash_set.create () in
  let rec loop () =
    let grid_location, distance = Queue.dequeue_exn queue in
    if Grid_location.equal finish_location grid_location
    then distance
    else if Hash_set.mem visited grid_location
    then loop ()
    else (
      Hash_set.add visited grid_location;
      List.iter
        [ 0, 1; 0, -1; 1, 0; -1, 0 ]
        ~f:(fun (row', col') ->
          let grid_location' = Grid_location.add_delta grid_location ~row' ~col' in
          try
            match grid.(grid_location'.row).(grid_location'.col) with
            | Space.Filled -> ()
            | Empty -> Queue.enqueue queue (grid_location', distance + 1)
          with
          | _ -> ());
      loop ())
  in
  loop ()
;;

let run ~filename =
  let%map bytes = Reader.file_lines filename in
  let grid = Grid.create () in
  List.take bytes 1024
  |> List.iter ~f:(fun s ->
    match String.split s ~on:',' with
    | [ col; row ] -> grid.(Int.of_string row).(Int.of_string col) <- Filled
    | _ -> failwith "unreachable");
  let steps =
    bfs
      grid
      ~start_location:{ Grid_location.row = 0; col = 0 }
      ~finish_location:{ Grid_location.row = 70; col = 70 }
  in
  print_s ([%sexp_of: int] steps)
;;
