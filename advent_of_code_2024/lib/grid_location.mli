open! Core
open! Async

type t =
  { row : int
  ; col : int
  }
[@@deriving compare, sexp_of]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t
