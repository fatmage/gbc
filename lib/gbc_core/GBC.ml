open Cartridge

let gbc_module rom : (module CGB.S) =
  let mbc_type =
    match String.get rom 0x147 |> int_of_char with
    | 0x00 ->
        print_endline "ROM_ONLY";
        ROM_ONLY
    | 0x01 | 0x02 | 0x03 ->
        print_endline "MBC1";
        MBC1
    | 0x05 | 0x06 ->
        print_endline "MBC2";
        MBC2
    | 0x08 | 0x09 ->
        print_endline "ROM_RAM";
        ROM_RAM
    | 0x19 | 0x1A | 0x1B | 0x1C | 0x1D | 0x1E ->
        print_endline "MBC5";
        MBC5
    | v -> Utils.fail_value "Cartridge type not implemented" v
  in
  let rom_banks =
    match String.get rom 0x148 |> int_of_char with
    | 0x00 -> 2
    | 0x01 -> 4
    | 0x02 -> 8
    | 0x03 -> 16
    | 0x04 -> 32
    | 0x05 -> 64
    | 0x06 -> 128
    | 0x07 -> 256
    | 0x08 -> 512
    | v -> Utils.fail_value "Invalid number of ROM banks" v
  in
  let ram_banks =
    match String.get rom 0x149 |> int_of_char with
    | 0x00 -> 0
    | 0x02 -> 1
    | 0x03 -> 4
    | 0x04 -> 16
    | 0x05 -> 8
    | v -> Utils.fail_value "Invalid number of RAM banks" v
  in
  let cgb =
    match String.get rom 0x143 |> int_of_char with
    | v when v land 0x80 > 0 -> true
    | _ -> false
  in
  let state_module : (module State.S) =
    match (cgb, mbc_type) with
    | false, _ -> failwith "DMG compatibility mode currently unsupported."
    | _, ROM_ONLY -> (module State.Make (Cartridge.No_RAM) (GPUmem.CGB_memory))
    | _, ROM_RAM -> (module State.Make (Cartridge.With_RAM) (GPUmem.CGB_memory))
    | _, MBC1 ->
        (module State.Make
                  ((val Cartridge.mbc1 rom_banks ram_banks))
                  (GPUmem.CGB_memory))
    | _, MBC2 -> (module State.Make ((val Cartridge.mbc2)) (GPUmem.CGB_memory))
    (* | _, MBC3 -> *)
    | _, MBC5 ->
        (module State.Make ((val Cartridge.mbc5 ram_banks)) (GPUmem.CGB_memory))
  in
  (module CGB.Make ((val state_module)))
