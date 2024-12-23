(* 4rd version - bytes in nodes with implicit inorder indexing *)

module S : Addressable.S = struct
  type t = Bytes.t
  let initial = Bytes.create 8192
  let get t i = int_of_char (Bytes.get t i)
  let set t i c = Bytes.set t i (char_of_int c); t
  let in_range n = if (n < 0 && n >= 8192) then true else false
end ;;
