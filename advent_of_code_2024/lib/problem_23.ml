open! Core
open! Async

let k = 13

module K_tuple = struct
  type t = string list [@@deriving equal, compare, hash, sexp_of]
end

let rec k_tuples_from_vertex ~acc ~edges_by_vertex ~k vertex : K_tuple.t list =
  let out_edges = Hashtbl.find_exn edges_by_vertex vertex in
  if List.for_all acc ~f:(Set.mem out_edges)
  then
    if k = 1
    then [ vertex :: acc ]
    else (
      let acc = vertex :: acc in
      let out_edges_to_explore, _, _ =
        (* Prevent duplicates *)
        Set.split out_edges vertex
      in
      Set.to_list out_edges_to_explore
      |> List.concat_map ~f:(k_tuples_from_vertex ~acc ~edges_by_vertex ~k:(k - 1)))
  else []
;;

let identify_k_interconnected edges_by_vertex =
  Hashtbl.keys edges_by_vertex
  |> List.concat_map ~f:(k_tuples_from_vertex ~acc:[] ~edges_by_vertex ~k)
;;

let run ~filename =
  let%map file_lines = Reader.file_lines filename in
  let edges =
    List.map file_lines ~f:(fun s ->
      match String.split s ~on:'-' with
      | [ v1; v2 ] -> v1, v2
      | _ -> failwith "unreachable")
  in
  let edges = edges @ List.map edges ~f:(fun (v1, v2) -> v2, v1) in
  let edges_by_vertex =
    String.Table.of_alist_multi edges |> Hashtbl.map ~f:String.Set.of_list
  in
  let k_tuples = identify_k_interconnected edges_by_vertex in
  let num_groups_with_t = ref 0 in
  List.iter k_tuples ~f:(fun k_tuple ->
    if List.exists k_tuple ~f:(String.is_prefix ~prefix:"t")
    then num_groups_with_t := !num_groups_with_t + 1);
  print_s ([%sexp_of: string list list * int] (k_tuples, !num_groups_with_t))
;;
