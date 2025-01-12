module type S = sig
  include Addressable.S

  (* val load_rom : Bytes.t -> int -> t  *)

end


type mbc =
  | No_MBC
  | MBC0
  | MBC1
  | MBC2
  | MBC3
  (* No MBC4, presumably because 4 is considered unlucky in Japan *)
  | MBC5
  | MBC6
  | MBC7



module MBC_no_RAM : S =
struct
  type t = bytes
  let initial = Bytes.empty

  let in_rom i = 0x0000 <= i && i <= 0xFFFF
  let in_ram i = 0xA000 <= i && i <= 0xBFFF
  let get m i =
    if in_rom i then
      Bytes.get m i |> Char.code
    else
      0xFF
  let set m i v = m

  let in_range i = in_rom i || in_ram i
end

module MBC_RAM : S =
struct
  module Bank = (val RAM.make_chunk 8192 0xA000)

  type t = bytes * Bank.t
  let initial = Bytes.empty, Bank.initial
  let in_rom i = 0x0000 <= i && i <= 0xFFFF
  let in_ram i = 0xA000 <= i && i <= 0xBFFF

  let get (rom, ram) i =
    if in_rom i then
      Bytes.get rom i |> int_of_char
    else
      Bank.get ram i

  let set (rom, ram) i v =
    if in_rom i then
      (rom, ram)
    else
      (rom, Bank.set ram i v)

  let in_range i = in_rom i || in_ram i

end

let mbc1_no_ram rom_banks : (module S) = (module struct

  type banking_mode = M0 | M1
  type t = { rom : bytes; rom_bank : int; ram_bank : int; mode : banking_mode }
  let initial = { rom = Bytes.empty; rom_bank = 0b00001; ram_bank = 0; mode = M0 }

  let in_low  i = 0x0000 <= i && i <= 0x3FFF
  let in_high i = 0x4000 <= i && i <= 0x7FFF
  let in_rom i = in_low i && in_high i
  let in_ram i = 0xA000 <= i && i <= 0xBFFF

  let bank_low m =
    match m.mode with
    | M0 -> 0
    | M1 when rom_banks <= 32 -> 0
    | M1 when rom_banks = 64 -> m.ram_bank land 0b1 lsl 5
    | M1 when rom_banks = 128 -> m.ram_bank land 0b11 lsl 5

  let bank_high m =
    match rom_banks with
    | n when n <= 32 -> m.rom_bank
    | n when n = 64 ->
      let b5 = m.ram_bank land 0b1 lsl 5 in
      b5 lor m.rom_bank
    | n when n = 128 ->
      let b56 = m.ram_bank land 0b11 lsl 5 in
      b56 lor m.rom_bank

  let get m i =
    match i with
    | _ when in_low i ->
      let low_bank_n = bank_low m in
      Bytes.get m.rom (0x4000 * low_bank_n + i) |> int_of_char
    | _ when in_high i ->
      let high_bank_n = bank_high m in
      Bytes.get m.rom (0x4000 * high_bank_n + (i - 0x4000)) |> int_of_char
    | _ -> 0xFF


  let set m i v = m

  let in_range i = in_rom i && in_ram i

end)
