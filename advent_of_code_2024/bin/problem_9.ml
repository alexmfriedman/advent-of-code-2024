open! Core
open! Async

module Disk = struct
  type t = { disk : int option array } [@@deriving sexp_of]

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
    { disk }
  ;;

  let rec has_k_spaces_free_at_idx t ~idx ~k =
    if k = 0
    then true
    else
      Option.is_none t.disk.(idx) && has_k_spaces_free_at_idx t ~idx:(idx + 1) ~k:(k - 1)
  ;;

  let insert_at_first_location_with_k_spaces_free t ~k ~first_file_id_idx ~file_id =
    let rec loop idx =
      if idx + k <= first_file_id_idx
      then
        if has_k_spaces_free_at_idx t ~idx ~k
        then
          for i = 0 to k - 1 do
            t.disk.(idx + i) <- Some file_id;
            t.disk.(first_file_id_idx + i) <- None
          done
        else loop (idx + 1)
    in
    loop 0
  ;;

  let rec file_id_length t ~file_id idx =
    if idx < Array.length t.disk && Option.exists t.disk.(idx) ~f:(Int.equal file_id)
    then 1 + file_id_length t ~file_id (idx + 1)
    else 0
  ;;
end

let run ~filename =
  let%map disk_map = Reader.file_lines filename >>| List.hd_exn in
  let disk = Disk.create ~disk_map in
  let idx = ref (Array.length disk.disk - 1) in
  let next_file_id = ref (disk.disk.(!idx) |> Option.value_exn) in
  while !idx > 0 do
    (match disk.disk.(!idx) with
     | Some file_id
       when Int.equal !next_file_id file_id
            && not (Option.exists disk.disk.(!idx - 1) ~f:(Int.equal !next_file_id)) ->
       let file_id_length = Disk.file_id_length disk ~file_id !idx in
       Disk.insert_at_first_location_with_k_spaces_free
         disk
         ~k:file_id_length
         ~first_file_id_idx:!idx
         ~file_id;
       next_file_id := !next_file_id - 1
     | _ -> ());
    idx := !idx - 1
  done;
  let result =
    Array.foldi disk.disk ~init:0 ~f:(fun i acc file_id ->
      acc + (i * Option.value file_id ~default:0))
  in
  print_s ([%sexp_of: int] result)
;;
