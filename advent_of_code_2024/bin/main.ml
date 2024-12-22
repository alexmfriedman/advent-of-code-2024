open! Core
open! Async

let cmd =
  Command.async
    ~summary:""
    (let%map_open.Command filename =
       flag "-filename" (required Filename_unix.arg_type) ~doc:"FILENAME Filename"
     and which_problem =
       flag "-which-problem" (required int) ~doc:"INT Which day number"
     in
     fun () ->
       match which_problem with
       | 1 -> Problem_1.run ~filename
       | 2 -> Problem_2.run ~filename
       | 3 -> Problem_3.run ~filename
       | 4 -> Problem_4.run ~filename
       | 5 -> Problem_5.run ~filename
       | 6 -> Problem_6.run ~filename
       | 7 -> Problem_7.run ~filename
       | 8 -> Problem_8.run ~filename
       | 9 -> Problem_9.run ~filename
       | 10 -> Problem_10.run ~filename
       | _ -> failwith "Unexpected problem #")
;;

let () = Command_unix.run cmd
