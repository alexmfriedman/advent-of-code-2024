open! Core
open! Async

module T = struct
  type t =
    { row : int
    ; col : int
    }
  [@@deriving hash, compare, sexp_of]
end

include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)
