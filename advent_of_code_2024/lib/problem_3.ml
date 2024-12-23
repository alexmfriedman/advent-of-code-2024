open! Core
open! Async

let mul_re =
  Re.Pcre.re "(mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\))" |> Re.compile
;;

let run ~filename =
  let%map file_contents = Reader.file_contents filename in
  let result, _enabled =
    Re.all mul_re file_contents
    |> List.fold ~init:(0, true) ~f:(fun (acc, enabled) group ->
      match Re.Group.get group 0, enabled with
      | "do()", _ -> acc, true
      | "don't()", _ -> acc, false
      | _, true ->
        (match Re.Group.all group with
         | [| _; _; x; y |] ->
           let acc = acc + (Int.of_string x * Int.of_string y) in
           acc, enabled
         | all ->
           let s = Array.to_list all |> String.concat ~sep:";" in
           failwith ("[" ^ s ^ "]"))
      | _, false -> acc, enabled)
  in
  print_endline (Int.to_string result)
;;
