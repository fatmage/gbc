
let log2int n = int_of_float (Float.round (Float.log2 (float_of_int n)))

let rec pow a =
  function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in  b * b * (if n mod 2 = 0 then 1 else a)

(* 4th version - bytes in nodes with implicit inorder indexing *)

let make_chunk size start :(module Addressable.S) =
  (module struct

  type t = Leaf | Node of int * t * int (* u8 *) * t | Cap of int (* u8 *) * t

  let rec count_nodes mem =
    match mem with
      | Leaf -> 0
      | Node (_,l,_,r) -> 1 + (count_nodes l) + (count_nodes r)
      | Cap (_,m) -> 1 + (count_nodes m)
  let empty =
    let rec help n i offset=
      match n with
        | 0 -> Leaf
        | n -> Node (i + offset, help (n-1) (i/2) offset, 0,
                                 help (n-1) (i/2) (offset + i))
    in
    Cap (0, help (log2int size) (pow 2 ((log2int size) - 1)) 0)

  let rec get mem index =
    match mem, index with
      | Cap (v,_), 0 -> v
      | Cap (_,m), _ -> get m index
      | Node (i,l,v,r), index ->
        if index == i then
        v else
        if index < i then
        get l index else
        get r index
      | _,_ -> failwith "no cap"

  let rec set mem index v =
    match mem, index with
      | Cap (_,m), 0 -> Cap (v,m)
      | Cap (x,m), _ -> Cap (x, set m index v)
      | Node (i,l,x,r), index ->
        if index == i then
        Node (i,l,v,r) else
        if index < i then
        Node (i,set l index v,x,r) else
        Node (i,l,x,set r index v)
      | _,_ -> failwith "no cap"

  let in_range n = n >= start && n < start + size

end)
    (* https://gbdev.io/pandocs/Memory_Map.html
    Memory map:
    0000	3FFF	16 KiB ROM bank 00	From cartridge, usually a fixed bank
    4000	7FFF	16 KiB ROM Bank 01–NN	From cartridge, switchable bank via mapper (if any)
    8000	9FFF	8 KiB Video RAM (VRAM)	In CGB mode, switchable bank 0/1
    A000	BFFF	8 KiB External RAM	From cartridge, switchable bank if any
    C000	CFFF	4 KiB Work RAM (WRAM)
    D000	DFFF	4 KiB Work RAM (WRAM)	In CGB mode, switchable bank 1–7
    E000	FDFF	Echo RAM (mirror of C000–DDFF)	Nintendo says use of this area is prohibited.
    FE00	FE9F	Object attribute memory (OAM)
    FEA0	FEFF	Not Usable	Nintendo says use of this area is prohibited.
    FF00	FF7F	I/O Registers
    FF80	FFFE	High RAM (HRAM)
    FFFF	FFFF	Interrupt Enable register (IE)
    *)


module type RAM = sig
  include Addressable.S
end

module S = struct
  module M = (val make_chunk 8912 0xA000)
  include M
end

module WRAM = struct
  module M = (val make_chunk 8912 0xC000)
  include M
end

module HRAM = struct
  module M = (val make_chunk 126 0xFF80)
  include M
end

module VRAM = struct
  module M = (val make_chunk 8912 0x8000)
  include M
end
