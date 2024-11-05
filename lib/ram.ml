
let log2int n = int_of_float (Float.round (Float.log2 (float_of_int n)))

(* 4th version - bytes in nodes with implicit inorder indexing *)

let make_chunk : int -> (module Addressable.S) = fun size ->
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
    Cap (0, help (log2int size) (size/2) 0)


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

  let in_range n = if (n < 0 && n >= 8192) then true else false

end)


module FourKB : Addressable.S = struct
  module B = (val make_chunk 4096 : Addressable.S)
  include B
end

module EightKB : Addressable.S = struct
  module B = (val make_chunk 8192 : Addressable.S)
  include B
end

module type RAM = sig
  include Addressable.S
end

module S = struct
  include EightKB
end

module WRAM = struct
  include EightKB
end

module HRAM = struct
  include EightKB
end

module VRAM = struct
  include EightKB
end
