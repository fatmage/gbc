module FourKB : Addressable.S
module EightKB : Addressable.S

module type RAM = sig
include Addressable.S
end

module S : RAM
module WRAM : RAM
module HRAM : RAM
module VRAM : RAM