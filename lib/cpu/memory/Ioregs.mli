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
  val inc_div : t -> t
  val inc_tima : t -> t * bool
  val tac_enabled : t -> bool
  val tima_mcyc : t -> int
  val mcyc_to_hz : int -> bool -> int
  val switch_speed : t -> t
  val run_div : t -> int -> t
  val run_tima : t -> int -> t * bool
end

module Interrupts : sig
  include Addressable.S

  val request_joypad : t -> t
  val request_serial : t -> t
  val request_timer : t -> t
  val request_LCD : t -> t
  val request_VBlank : t -> t
end

module Audio : sig
  include Addressable.S
end

module WavePattern : sig
  include Addressable.S
end

module IE : sig
  include Addressable.S
end