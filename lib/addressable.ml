module type S = sig
  type t
  val empty : t
  val get : t -> int -> int (* u8 *)
  val set : t -> int -> int (* u8 *) -> t
  val in_range : int -> bool
end