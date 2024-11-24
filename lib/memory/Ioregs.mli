(*
  $FF00   	      Joypad input
  $FF01   $FF02   Serial transfer
  $FF04   $FF07   Timer and divider
  $FF0F           Interrupts
  $FF10   $FF26   Audio
  $FF30   $FF3F   Wave pattern
  $FF40   $FF4B   LCD Control, Status, Position, Scrolling, and Palettes
  $FF4F           VRAM Bank Select
  $FF50           Set to non-zero to disable boot ROM
  $FF51   $FF55   VRAM DMA
  $FF68   $FF6B   BG / OBJ Palettes
  $FF70   	      WRAM Bank Select
*)

module Joypad : sig
  include Addressable.S
end

module Serial : sig
  include Addressable.S
end

module Timer : sig
  include Addressable.S

  val reset_div : t -> t
end

module Interrupts : sig
  include Addressable.S
end

module Audio : sig
  include Addressable.S
end

module WavePattern : sig
  include Addressable.S
end

module LCDControl : sig
  include Addressable.S
end

module Palettes : sig
  include Addressable.S
end

module IE : sig
  include Addressable.S
end