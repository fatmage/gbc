open Inttypes


(* 4rd version - bytes in nodes with implicit inorder indexing *)

module S : Addressable.S = struct

  type t = Bytes.t

  let empty = Bytes.create 8192

  let get t i = Inttypes.char_to_u8 (Bytes.get t i)

  let set t i c = Bytes.set t i (u8_to_char c); t

  let in_range n = if (n < 0 && n >= 8192) then true else false

end ;;
