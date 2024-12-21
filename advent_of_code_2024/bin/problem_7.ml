open! Core
open! Async

let operations =
  [ ( + ); ( * ); (fun x y -> Int.to_string x ^ Int.to_string y |> Int.of_string) ]
;;

let rec equation_valid ~test_value ~curr_value = function
  | [] -> test_value = curr_value
  | v :: vs ->
    List.exists operations ~f:(fun f ->
      equation_valid ~test_value ~curr_value:(f curr_value v) vs)
;;

(* NB: You could DP this, but the lines are small enough it won't make a difference in practice *)
let equation_valid ~test_value ~values =
  match values with
  | [] -> failwith "unreachable"
  | v :: vs -> equation_valid ~test_value ~curr_value:v vs
;;

let run ~filename =
  let total = ref 0 in
  let%map () =
    Reader.with_file filename ~f:(fun reader ->
      Reader.lines reader
      |> Pipe.iter_without_pushback ~f:(fun s ->
        let idx = String.substr_index_exn s ~pattern:": " in
        let test_value = String.prefix s idx |> Int.of_string in
        let values =
          String.drop_prefix s (idx + 2)
          |> String.split ~on:' '
          |> List.map ~f:Int.of_string
        in
        if equation_valid ~test_value ~values then total := !total + test_value))
  in
  print_s ([%sexp_of: int] !total)
;;
