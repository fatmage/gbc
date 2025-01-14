open Cartridge

let gbc_module rom : (module CPU.S)=
  let mbc_type =
    match Bytes.get rom 0x147 |> int_of_char with
    | 0x00               -> ROM_ONLY
    | 0x01 | 0x02 | 0x03 -> MBC1
    | 0x05 | 0x06        -> MBC2
    | 0x08 | 0x09        -> ROM_RAM
    | 0x19 | 0x1A | 0x1B
    | 0x1C | 0x1D | 0x1E -> MBC5
    | _ -> failwith "Cartridge type not implemented."
  in
  let rom_banks =
    match Bytes.get rom 0x148 |> int_of_char with
    | 0x00 -> 2
    | 0x01 -> 4
    | 0x02 -> 8
    | 0x03 -> 16
    | 0x04 -> 32
    | 0x05 -> 64
    | 0x06 -> 128
    | 0x07 -> 256
    | 0x08 -> 512
    | _ -> failwith "Invalid number of ROM banks."
  in
  let ram_banks =
    match Bytes.get rom 0x149 |> int_of_char with
    | 0x00 -> 0
    | 0x02 -> 1
    | 0x03 -> 4
    | 0x04 -> 16
    | 0x05 -> 8
    | _ -> failwith "Invalid number of RAM banks."
  in
  let cgb =
    match Bytes.get rom 0x143 |> int_of_char with
    | v when (v land 0x80) > 0 -> true
    | _ -> false
  in
  let state_module : (module State.S) =
    match cgb, mbc_type with
    | false, _ -> failwith "DMG compatibility mode currently unsupported."
    | _, ROM_ONLY -> (module State.Make(Cartridge.No_RAM)(GPUmem.CGB_memory))
    | _ , ROM_RAM -> (module State.Make(Cartridge.With_RAM)(GPUmem.CGB_memory))
    | _, MBC1     -> (module State.Make(val Cartridge.mbc1 rom_banks ram_banks)(GPUmem.CGB_memory))
    | _, MBC2     -> (module State.Make(val Cartridge.mbc2 rom_banks)(GPUmem.CGB_memory))
    (* | _, MBC3 -> *)
    | _, MBC5     -> (module State.Make(val Cartridge.mbc5 rom_banks ram_banks)(GPUmem.CGB_memory))
  in
  (module CPU.Make(val state_module))
