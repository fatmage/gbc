let set_low_byte u16 u8 = (u16 land 0xFF) lor u8
let set_high_byte u16 u8 = (u16 land 0x00FF) lor (u8 lsl 8)
let get_high u16 = u16 lsr 8
let get_low u16 = u16 land 0xFF

let u8 = (land) 0xFF
let u16 = (land) 0xFFFF
