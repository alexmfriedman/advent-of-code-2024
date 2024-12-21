open! Core
open! Async

module Space = struct
  type t =
    | Empty [@rename "."]
    | Obstacle [@rename "#"]
  [@@deriving string]
end

let run ~filename =
  let%map _file_contents = Reader.file_lines filename in
  failwith "NYI"
;;
