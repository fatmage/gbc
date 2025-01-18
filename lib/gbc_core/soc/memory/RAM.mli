
module type S = sig
include Addressable.S
end

val make_chunk : int -> int -> (module Addressable.S)

module WRAM : sig
  include S
  val in_echo : int -> bool
end
module HRAM : S
