open! Core
open! Async

module type S = sig
  val run : filename:Filename.t -> unit Deferred.t
end

module type Problem = sig
  module type S = S
end
