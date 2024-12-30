open! Core
open! Async

let read_result_ok_exn : 'a Reader.Read_result.t -> 'a = function
  | `Ok v -> v
  | `Eof -> failwith "unreachable"
;;

let parse_lock l =
  List.tl_exn l
  |> List.map ~f:String.to_list
  |> List.transpose_exn
  |> List.map ~f:(fun down -> List.findi_exn down ~f:(fun _i -> Char.equal '.') |> fst)
;;

let parse_lines l =
  let fst = List.hd_exn l in
  (* Locks go down*)
  if String.equal fst "#####"
  then `Lock (parse_lock l)
  else `Key (List.rev l |> parse_lock)
;;

let run ~filename =
  let keys = Queue.create () in
  let locks = Queue.create () in
  let%map () =
    Reader.with_file filename ~f:(fun reader ->
      Deferred.repeat_until_finished () (fun () ->
        let%bind key_or_lock_lines =
          Deferred.List.init ~how:`Sequential 7 ~f:(fun _ ->
            Reader.read_line reader >>| read_result_ok_exn)
        in
        (match parse_lines key_or_lock_lines with
         | `Key key -> Queue.enqueue keys key
         | `Lock lock -> Queue.enqueue locks lock);
        match%map Reader.read_line reader with
        | `Eof -> `Finished ()
        | `Ok "" -> `Repeat ()
        | `Ok _ -> failwith "unreachable"))
  in
  let total =
    List.cartesian_product (Queue.to_list keys) (Queue.to_list locks)
    |> List.sum
         (module Int)
         ~f:(fun (key, lock) ->
           if List.zip_exn key lock |> List.for_all ~f:(fun (k, l) -> k + l <= 5)
           then 1
           else 0)
  in
  print_s ([%sexp_of: int] total)
;;
