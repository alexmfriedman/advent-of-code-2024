open! Core
open! Async

module Disk = struct
  type t =
    { disk : int option array
    ; mutable start_free_search_at : int
    }
  [@@deriving sexp_of]

  let create ~disk_map =
    let disk =
      List.init
        ((String.length disk_map / 2) + 1)
        ~f:(fun i ->
          let file_size =
            String.get disk_map (2 * i) |> Char.to_string |> Int.of_string
          in
          let free_space_size =
            Option.try_with (fun () -> String.get disk_map ((2 * i) + 1))
            |> Option.value_map ~f:(fun c -> Char.to_string c |> Int.of_string) ~default:0
          in
          Array.append
            (Array.create ~len:file_size (Some i))
            (Array.create ~len:free_space_size None))
      |> Array.concat
    in
    { disk; start_free_search_at = 0 }
  ;;

  let move_to_first_free_exn t ~idx =
    let rec loop () =
      if idx < t.start_free_search_at
      then ()
      else (
        match t.disk.(t.start_free_search_at) with
        | None ->
          t.disk.(t.start_free_search_at) <- t.disk.(idx);
          t.disk.(idx) <- None;
          t.start_free_search_at <- t.start_free_search_at + 1
        | Some _ ->
          t.start_free_search_at <- t.start_free_search_at + 1;
          loop ())
    in
    loop ()
  ;;
end

let run ~filename =
  let%map disk_map = Reader.file_lines filename >>| List.hd_exn in
  let disk = Disk.create ~disk_map in
  let idx = ref (Array.length disk.disk - 1) in
  while !idx >= 0 && !idx > disk.start_free_search_at do
    (match disk.disk.(!idx) with
     | None -> ()
     | Some _file_id -> Disk.move_to_first_free_exn disk ~idx:!idx);
    idx := !idx - 1
  done;
  let result =
    Array.foldi disk.disk ~init:0 ~f:(fun i acc file_id ->
      acc + (i * Option.value file_id ~default:0))
  in
  print_s ([%sexp_of: int] result)
;;
