open Inttypes

module type S = sig
  type t
  val empty : t
  val get : t -> int -> uint8
  val set : t -> int -> uint8 -> t
  val in_range : int -> bool
end