let set_low_byte u16 u8 = (u16 land 0xFF00) lor u8
let set_high_byte u16 u8 = (u16 land 0x00FF) lor (u8 lsl 8)
let get_high u16 = u16 lsr 8
let get_low u16 = u16 land 0xFF

let u8 i = i land 0xFF
let u16 i = i land 0xFFFF
let s8 n = (n land 0x7F) - (n land 0x80)

let rev_u8 u8 =
  let rec loop acc u8 =
    function
    | 0 -> acc
    | n -> loop ((acc lsl 1) lor (u8 land 1)) (u8 lsr 1) (n-1) in
    loop 0 u8 8


let fail_value msg v = Printf.sprintf "%s Value: 0x%04X" msg v |> failwith
let fail_addr msg addr = Printf.sprintf "%s Address: 0x%04X" msg addr |> failwith
let unreachable () = failwith "Unreachable case."

let print_hex msg v = Printf.sprintf "%s: 0x%04X" msg v |> print_endline
let value_hex v = Printf.sprintf "Value: 0x%08X" v |> print_endline
let print_dec msg v = Printf.sprintf "%s: %d" msg v |> print_endline
