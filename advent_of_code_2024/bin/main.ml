open! Core
open! Async

let cmd =
  Command.async ~summary:""
    (let%map_open.Command filename =
       flag "-filename"
         (required Filename_unix.arg_type)
         ~doc:"FILENAME Filename"
     and which_problem =
       flag "-which-problem" (required int) ~doc:"INT Which day number"
     in
     fun () ->
       match which_problem with
       | 1 -> Problem_1.run ~filename
       | 2 -> Problem_2.run ~filename
       | _ -> failwith "Unexpected problem #")

let () = Command_unix.run cmd
