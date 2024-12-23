
module type RAM = sig
include Addressable.S
end

val make_chunk : int -> int -> (module Addressable.S)

module RAM : RAM
module WRAM : RAM
module HRAM : RAM
