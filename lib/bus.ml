type t = {rom : Rom.S.t; ram : Ramchunk.S.t (* regfile*) (* joypad ?*)}

let get t i =
  match i with
  | i when i <= 0
    -> failwith "Bus error: can't get memory at negative address."
  | i when i < 8192
    -> Rom.S.get t.rom i
  | i when i < 16384
    -> Ramchunk.S.get t.ram i
  | _
    -> failwith "Bus error: address out of range."


let set t i v =
  match i with
  | i when i <= 0
    -> failwith "Bus error: can't get memory at negative address."
  | i when i < 8192
    -> {t with rom = Rom.S.set t.rom i v}
  | i when i < 16384
    -> {t with ram = Ramchunk.S.set t.ram i v}
  | _
    -> failwith "Bus error: address out of range."