module type S = sig
  type t

  val initial : t
  val get : t -> int -> int
  val set : t -> int -> int -> t
  val in_range : int -> bool
end
