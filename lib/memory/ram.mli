
module type RAM = sig
include Addressable.S
end

module RAM : RAM
module WRAM : RAM
module HRAM : RAM
module VRAM : RAM
