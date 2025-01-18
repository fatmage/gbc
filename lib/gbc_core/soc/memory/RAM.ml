
let log2int n = int_of_float (Float.round (Float.log2 (float_of_int n)))

let rec pow a =
  function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in  b * b * (if n mod 2 = 0 then 1 else a)

(* 4th version - bytes in nodes with implicit inorder indexing *)

let make_chunk size start : (module Addressable.S) =
  (module struct

  type t = Leaf | Node of int * t * int (* u8 *) * t | Cap of int (* u8 *) * t

  let initial =
    let rec help n i offset=
      match n with
        | 0 -> Leaf
        | n -> Node (i + offset, help (n-1) (i/2) offset, 0,
                                 help (n-1) (i/2) (offset + i))
    in
    Cap (0, help (log2int size) (pow 2 ((log2int size) - 1)) 0)

  let get mem index =
    let index = index - start in
    let rec aux mem =
      match mem, index with
        | Cap (v,_), 0 -> v
        | Cap (_,m), _ -> aux m
        | Node (i,l,v,r), index ->
          if index == i then
          v else
          if index < i then
          aux l else
          aux r
        | _,_ -> Utils.fail_addr "no cap" index
    in
    aux mem

  let set mem index v =
    let index = index - start in
    let rec aux mem =
      match mem, index with
        | Cap (_,m), 0 -> Cap (v,m)
        | Cap (x,m), _ -> Cap (x, aux m)
        | Node (i,l,x,r), index ->
          if index == i then
          Node (i,l,v,r) else
          if index < i then
          Node (i,aux l,x,r) else
          Node (i,l,x,aux r)
        | _,_ -> Utils.fail_addr "no cap" index
    in
    aux mem

  let in_range n = start <= n && n < start + size

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


module type S = sig
  include Addressable.S
end


module WRAM = struct
  module Bank0 = (val make_chunk 4096 0xC000)
  module Banks = (val make_chunk 4096 0xD000)

  type t = { b0: Bank0.t; bs: Banks.t list; svbk : int }

  let initial =
    { b0 = Bank0.initial; bs = List.init 7 (fun _ -> Banks.initial); svbk = 0xF8 }

  let get m i =
    match m,i with
    | {svbk; _}, 0xFF70 -> svbk
    | {b0; _}, i when Bank0.in_range i -> Bank0.get b0 i
    | {bs=(b::_);_}, i -> Banks.get b i
    | _    -> assert false

  let rec rot (x::xs) =
    function
    | 0            -> x::xs
    | i when i < 0 -> rot (xs @ [x]) (i + 1)
    | i            -> rot (x::xs) (7 - i)

  let set m i v =
    match m,i with
    | {b0;bs;svbk}, 0xFF70 ->
      if v land 0b111 = svbk land 0b111 then
        { m with svbk=v }
      else
        let diff = svbk land 0b111 - v land 0b111 in
        { b0; bs = rot bs diff; svbk=v }
    | {b0;_}, i when Bank0.in_range i ->
      { m with b0 = Bank0.set b0 i v }
    | {bs = b::bs; _}, i ->
      { m with bs = Banks.set b i v :: bs }
    | _    -> assert false

  let in_range i = Bank0.in_range i || Banks.in_range i || i = 0xFF70

  let in_echo i = 0xC000 <= i && i <= 0xDDFF

end

module HRAM = struct
  module M = (val make_chunk 127 0xFF80)
  include M
end
