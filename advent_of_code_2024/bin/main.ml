open! Core
open! Async
open Advent_of_code_2024

let solutions =
  [ Problem_1.run
  ; Problem_2.run
  ; Problem_3.run
  ; Problem_4.run
  ; Problem_5.run
  ; Problem_6.run
  ; Problem_7.run
  ; Problem_8.run
  ; Problem_9.run
  ; Problem_10.run
  ; Problem_11.run
  ; Problem_12.run
  ; Problem_13.run
  ; Problem_14.run
  ; Problem_15.run
  ; Problem_16.run
  ]
;;

let cmd =
  Command.async
    ~summary:""
    (let%map_open.Command filename =
       flag "-filename" (required Filename_unix.arg_type) ~doc:"FILENAME Filename"
     and which_problem =
       flag "-which-problem" (required int) ~doc:"INT Which day number"
     in
     fun () -> (List.nth_exn solutions (which_problem - 1)) ~filename)
;;

let () = Command_unix.run cmd
