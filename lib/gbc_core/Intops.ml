let set_low_byte u16 u8 = (u16 land 0xFF) lor u8
let set_high_byte u16 u8 = (u16 land 0x00FF) lor (u8 lsl 8)
let get_high u16 = u16 lsr 8
let get_low u16 = u16 land 0xFF

let u8 = (land) 0xFF
let u16 = (land) 0xFFFF
let s8 n = n land 0x7F - (n land 0x80)

let rev_u8 u8 =
  let rec loop acc u8 =
    function
    | 0 -> acc
    | n -> loop ((acc lsl 1) lor (u8 land 1)) (u8 lsr 1) (n-1) in
    loop 0 u8 8
