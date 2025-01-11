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
      Bytes.get rom i |> Char.code
    else
      Bank.get ram i

  let set (rom, ram) i v =
    if in_rom i then
      (rom, ram)
    else
      (rom, Bank.set ram i v)

  let in_range i = in_rom i || in_ram i

end

let mbc_no_ram rom_banks : (module S) = (module struct
  type t = bytes * bank
  let initial = Bytes.empty, 0b00001
  let in_rom i = 0x0000 <= i && i <= 0xFFFF
  let in_ram i = 0xA000 <= i && i <= 0xBFFF

  let

end)

module MBC1_no_RAM : S =
end