open! Core
open! Async

let run ~filename =
  let l1 = Queue.create () in
  let l2 = Queue.create () in
  let%map () =
    Reader.with_file filename ~f:(fun reader ->
      Reader.lines reader
      |> Pipe.iter ~f:(fun s ->
        (match
           String.split ~on:' ' s
           |> List.filter ~f:(Fn.non String.is_empty)
           |> List.map ~f:Int.of_string
         with
         | [ e1; e2 ] ->
           Queue.enqueue l1 e1;
           Queue.enqueue l2 e2
         | _ -> failwith "bad");
        Deferred.unit))
  in
  let right_list_counts =
    Queue.to_list l2
    |> Sequence.of_list
    |> Sequence.map ~f:(fun elem -> elem, 1)
    |> Int.Map.of_sequence_reduce ~f:( + )
  in
  let result =
    Queue.fold l1 ~init:0 ~f:(fun acc e ->
      let points = Map.find right_list_counts e |> Option.value ~default:0 in
      acc + (e * points))
  in
  print_int result
;;
