(* 4rd version - bytes in nodes with implicit inorder indexing *)

module type S = sig
  include Addressable.S
end


let make_rom (placeholder : bool) : (module S) = (
module struct

type t = Bytes.t
let initial = Bytes.create 8192

let get m i = int_of_char (Bytes.get m i)

let set m i v = m

let in_range n = n >= 0 && n < 8192
end)

module Default = struct
  module M = (val make_rom true)
  include M
end
