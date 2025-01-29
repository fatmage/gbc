module type S = sig
  include Addressable.S

  val load_rom : t -> bytes -> t

end


type mbc =
  | ROM_ONLY
  | ROM_RAM
  | MBC1
  | MBC2
  | MBC3
  (* No MBC4, presumably because 4 is considered unlucky in Japan *)
  | MBC5
  | MBC6
  | MBC7



module No_RAM : S =
struct
  type t = bytes
  let initial = Bytes.empty

  let in_rom i = 0x0000 <= i && i <= 0x7FFF
  let in_ram i = 0xA000 <= i && i <= 0xBFFF
  let get m i =
    if in_rom i then
      Bytes.get m i |> Char.code
    else
      0xFF
  let set m _ _ = m

  let load_rom _ rom = rom
  let in_range i = in_rom i || in_ram i
end

module With_RAM : S =
struct
  module Bank = (val RAM.make_chunk 8192 0xA000)

  type t = bytes * Bank.t
  let initial = Bytes.empty, Bank.initial
  let in_rom i = 0x0000 <= i && i <= 0x7FFF
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

  let load_rom (_,ram) rom = rom,ram

  let in_range i = in_rom i || in_ram i
end


let mbc1 rom_banks ram_banks : (module S) = (module struct

  module RAM = (val RAM.make_chunk (ram_banks * 8192) 0xA000)

  type banking_mode = M0 | M1
  type t = { rom : bytes; ram: RAM.t; rom_bank : int; ram_bank : int; mode : banking_mode; ram_enabled : bool }
  let initial = { rom = Bytes.empty; ram = RAM.initial; rom_bank = 0b00001; ram_bank = 0b00; mode = M0; ram_enabled = false }

  let in_low  i = 0x0000 <= i && i <= 0x3FFF
  let in_high i = 0x4000 <= i && i <= 0x7FFF
  let in_rom i = in_low i || in_high i
  let in_ram i = 0xA000 <= i && i <= 0xBFFF

  let bank_low m =
    match m.mode with
    | M0 -> 0
    | M1 when rom_banks <= 32 -> 0
    | M1 when rom_banks = 64 -> (m.ram_bank land 0b1) lsl 5
    | M1 when rom_banks = 128 -> (m.ram_bank land 0b11) lsl 5

  let addr_low m i = 0x4000 * (bank_low m) + i

  let bank_high m =
    match rom_banks with
    | n when n <= 32 -> m.rom_bank
    | n when n = 64 ->
      let b5 = (m.ram_bank land 0b1) lsl 5 in
      b5 lor m.rom_bank
    | n when n = 128 ->
      let b56 = (m.ram_bank land 0b11) lsl 5 in
      b56 lor m.rom_bank

  let addr_high m i = 0x4000 * (bank_high m) + i - 0x4000

  let ram_addr m i =
    match m.mode, ram_banks with
    | M0, _ | M1, 1 -> i
    | M1, 4 -> 0x2000 * ram_banks + i

  let get m i =
    match i with
    | _ when in_low i  ->
      Bytes.get m.rom (addr_low m i) |> int_of_char
    | _ when in_high i ->
      Bytes.get m.rom (addr_high m i) |> int_of_char
    | _ when m.ram_enabled ->
      RAM.get m.ram (ram_addr m i)
    | _ -> 0xFF

  let rom_size_bitmask =
    function
    | 2   -> 0b00000001
    | 4   -> 0b00000011
    | 8   -> 0b00000111
    | 16  -> 0b00001111
    | 32  -> 0b00011111
    | 64  -> 0b00011111
    | 128 -> 0b00011111

  let set m i v =
    match i with
    | _ when 0x0000 <= i && i <= 0x1FFF ->
      begin match ram_banks, v with
      | 0, _   -> m
      | _, 0xA -> { m with ram_enabled = true }
      | _, _   -> m
      end
    | _ when 0x2000 <= i && i <= 0x3FFF ->
      let rom_bank = v land (rom_size_bitmask rom_banks) in
      let rom_bank = if rom_bank = 0 then 1 else rom_bank in
      { m with rom_bank }
    | _ when 0x4000 <= i && i <= 0x5FFF ->
      { m with ram_bank = v land 0b11 }
    | _ when 0x6000 <= i && i <= 0x7FFF ->
      { m with mode = if v land 1 = 0 then M0 else M1 }

    | _ when in_ram i && m.ram_enabled ->
      { m with ram = RAM.set m.ram (ram_addr m i) v }
    | _ -> m

  let load_rom m rom = { m with rom }

  let in_range i = in_rom i || in_ram i
end)

let mbc2 : (module S) = (module struct
module RAM = (val RAM.make_chunk 512 0xA000)
  type t = {rom : bytes; ram : RAM.t; rom_bank: int; ram_enabled : bool }
  let initial = { rom = Bytes.empty; ram = RAM.initial; rom_bank = 1; ram_enabled = false }

  let in_low  i = 0x0000 <= i && i <= 0x3FFF
  let in_high i = 0x4000 <= i && i <= 0x7FFF
  let in_rom i = in_low i || in_high i
  let in_ram i = 0xA000 <= i && i <= 0xBFFF


  let get m i =
    match i with
    | _ when in_low i ->
      Bytes.get m.rom i |> int_of_char
    | _ when in_high i ->
      Bytes.get m.rom (0x4000 * m.rom_bank + i - 0x4000) |> int_of_char
    | _ when m.ram_enabled ->
      RAM.get m.ram (0xA000 + (i land 0x1F))
    | _ ->
      0xFF

  let set m i v =
    match i with
    | _ when in_low i ->
      if i land (0b1 lsl 8) = 0 then
        if v = 0x0A then
          { m with ram_enabled = true }
        else
          { m with ram_enabled = false }
      else
        let bank = v land 0b1111 in
        { m with rom_bank = if bank = 0 then 1 else bank }
    | _ when in_ram i ->
      { m with ram = RAM.set m.ram (0xA000 + (i land 0x1F)) (v land 0b1111) }
    | _ -> m

  let load_rom m rom = { m with rom }

  let in_range i = in_rom i || in_ram i
end)

(*
  Skipping MBC3 for now, don't want to save state or count time
  outside the emulator for now
*)

(* No MBC4 *)

let mbc5 ram_banks : (module S) = (module struct

  module RAM = (val RAM.make_chunk (ram_banks * 8192) 0xA000)

  type t = { rom : bytes; ram : RAM.t; rom_bank : int; ram_bank : int; ram_enabled : bool }
  let initial = { rom = Bytes.empty; ram = RAM.initial; rom_bank = 1; ram_bank = 0; ram_enabled = false }

  let in_low  i = 0x0000 <= i && i <= 0x3FFF
  let in_high i = 0x4000 <= i && i <= 0x7FFF
  let in_rom i = in_low i || in_high i
  let in_ram i = 0xA000 <= i && i <= 0xBFFF

  let addr_high m i = 0x4000 * m.rom_bank + i - 0x4000

  let addr_ram m i =
    match ram_banks with
    | 0 -> i
    | _ ->
      let ram_bank = m.ram_bank mod ram_banks in
      ram_bank * (i - 0xA000) + 0xA000

  let get m i =
    match i with
    | _ when in_low i ->
      Bytes.get m.rom i |> int_of_char
    | _ when in_high i ->
      Bytes.get m.rom (addr_high m i) |> int_of_char
    | _ (* when in_ram i *) ->
      if m.ram_enabled then RAM.get m.ram (addr_ram m i) else 0xFF


  let set m i v =
    match i with
    | _ when 0x0000 <= i && i <= 0x1FFF ->
      begin match v with
      | 0x0A -> { m with ram_enabled = true }
      | _    -> { m with ram_enabled = false }
      end
    | _ when 0x2000 <= i && i <= 0x2FFF ->
      { m with rom_bank = (m.rom_bank land (lnot 0xFF)) lor v }
    | _ when 0x3000 <= i && i <= 0x3FFF ->
      { m with rom_bank = (m.rom_bank land 0xFF) lor (v land 0b1 lsl 8) }
    | _ when 0x4000 <= i && i <= 0x5FFF ->
      { m with ram_bank = v land 0x0F }
    | _ when in_ram i ->
      { m with ram = RAM.set m.ram (addr_ram m i) v }

  let load_rom m rom = { m with rom }

  let in_range i = in_rom i || in_ram i
end)
