open! Core
open! Async

let construct_re ~towels =
  let towel_options = String.concat ~sep:"|" towels in
  sprintf "^(%s)*$" towel_options
;;

let run ~filename =
  let%map file_lines = Reader.file_lines filename in
  let towels =
    List.hd_exn file_lines |> String.split ~on:',' |> List.map ~f:String.strip
  in
  let re_string = construct_re ~towels in
  let re = Re.Pcre.re re_string |> Re.compile in
  let patterns = List.drop file_lines 2 in
  let num_possible_designs =
    List.sum
      (module Int)
      patterns
      ~f:(fun pattern ->
        print_s ([%sexp_of: string * string] (re_string, pattern));
        Re.all re pattern |> List.length)
  in
  print_s ([%sexp_of: int] num_possible_designs)
;;
