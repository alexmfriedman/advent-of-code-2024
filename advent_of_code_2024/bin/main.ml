open! Core
open! Async

let cmd =
  Command.async ~summary:""
    (let%map_open.Command () = return () in
     fun () -> Deferred.unit)

let () = Command_unix.run cmd
