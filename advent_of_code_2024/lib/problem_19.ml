open! Core
open! Async

let towel_matches_pattern_substring ~towel ~target ~start_idx =
  start_idx + String.length towel <= String.length target
  && String.for_alli towel ~f:(fun i c ->
    Char.equal c (String.get target (start_idx + i)))
;;

let num_possible_designs ~towels ~target =
  let dp =
    Memo.recursive ~hashable:Int.hashable (fun dp idx ->
      if idx = String.length target
      then 1
      else
        List.sum
          (module Int)
          towels
          ~f:(fun towel ->
            if towel_matches_pattern_substring ~towel ~target ~start_idx:idx
            then dp (idx + String.length towel)
            else 0))
  in
  dp 0
;;

let run ~filename =
  let%map file_lines = Reader.file_lines filename in
  let towels =
    List.hd_exn file_lines |> String.split ~on:',' |> List.map ~f:String.strip
  in
  let targets = List.drop file_lines 2 in
  let num_possible_designs =
    List.sum (module Int) targets ~f:(fun target -> num_possible_designs ~towels ~target)
  in
  print_s ([%sexp_of: int] num_possible_designs)
;;
