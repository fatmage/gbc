
module type S = sig
include Addressable.S
end

val make_chunk : int -> int -> (module Addressable.S)

module WRAM : S
module HRAM : S
