open! Core
open! Async

module Rule = struct
  type t =
    { a : int
    ; b : int
    }
end

let is_in_right_order ~rules ~ints =
  let int_locs = List.mapi ints ~f:(fun i n -> n, i) |> Int.Table.of_alist_exn in
  Queue.for_all rules ~f:(fun { Rule.a; b } ->
    match Hashtbl.find int_locs a, Hashtbl.find int_locs b with
    | Some a_loc, Some b_loc -> a_loc < b_loc
    | _ -> true)
;;

let rec run_dfs ~rules_by_source ~remaining_ints source =
  if not (Hash_set.mem remaining_ints source)
  then []
  else (
    Hash_set.remove remaining_ints source;
    source
    :: (Map.find rules_by_source source
        |> Option.value ~default:[]
        |> List.map ~f:(run_dfs ~rules_by_source ~remaining_ints)
        |> List.rev
        |> List.concat))
;;

let run_topological_sort ~rules ~ints =
  let rules =
    List.filter rules ~f:(fun { Rule.a; b } ->
      List.mem ~equal:Int.equal ints a && List.mem ~equal:Int.equal ints b)
  in
  let rules_by_source =
    Sequence.of_list rules
    |> Sequence.map ~f:(fun { Rule.a; b } -> a, b)
    |> Int.Map.of_sequence_multi
  in
  let rules_by_dest =
    Sequence.of_list rules
    |> Sequence.map ~f:(fun { Rule.a; b } -> b, a)
    |> Int.Map.of_sequence_multi
  in
  let sources = List.filter ints ~f:(Fn.non (Map.mem rules_by_dest)) in
  let remaining_ints = Int.Hash_set.of_list ints in
  List.map sources ~f:(run_dfs ~rules_by_source ~remaining_ints)
  |> List.rev
  |> List.concat
;;

let run ~filename =
  let rules = Queue.create () in
  let sum = ref 0 in
  let%map () =
    Reader.with_file filename ~f:(fun reader ->
      Reader.lines reader
      |> Pipe.iter ~f:(fun s ->
        if String.contains s '|'
        then (
          match String.split ~on:'|' s |> List.map ~f:Int.of_string with
          | [ a; b ] -> Queue.enqueue rules { Rule.a; b }
          | _ -> failwith "bad")
        else if String.is_empty s
        then ()
        else (
          let ints = String.split s ~on:',' |> List.map ~f:Int.of_string in
          let ints_is_in_right_order = is_in_right_order ~rules ~ints in
          if not ints_is_in_right_order
          then (
            let ints' = run_topological_sort ~rules:(Queue.to_list rules) ~ints in
            assert (is_in_right_order ~rules ~ints:ints');
            sum := !sum + List.nth_exn ints' (List.length ints' / 2)));
        Deferred.unit))
  in
  print_endline (Int.to_string !sum)
;;
