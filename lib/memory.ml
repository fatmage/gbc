open Inttypes


module type MemoryChunk = sig
  
  type mem
  val empty : mem 
  val get : int -> mem -> uint8 
  val set : uint8 -> int -> mem -> mem 

end

(* 4rd version - bytes in nodes with implicit inorder indexing *)

module Mem : MemoryChunk = struct 

  type mem = Leaf | Node of int * mem * uint8 * mem | Cap of uint8 * mem

  let empty = 
    let rec help n i offset=
      match n with 
        | 0 -> Leaf
        | n -> Node (i + offset, help (n-1) (i/2) offset, U8.zero, 
                                 help (n-1) (i/2) (offset + i))
    in 
    Cap (U8.zero, help 12 2048 0)


  let rec get index mem =
    match mem, index with 
      | Cap (v,_), 0 -> v 
      | Cap (_,m), _ -> get index m
      | Node (i,l,v,r), index ->  if index == i then
                                    v else
                                    if index < i then 
                                      get index l else 
                                      get index r
      | _,_ -> failwith "no cap"

  let rec set v index mem =
    match mem, index with 
      | Cap (_,m), 0 -> Cap (v,m)
      | Cap (x,m), _ -> Cap (x, set v index m )
      | Node (i,l,x,r), index ->  if index == i then
                                    Node (i,l,v,r) else
                                    if index < i then 
                                      Node (i,set v index l,x,r) else 
                                      Node (i,l,x,set v index r)
      | _,_ -> failwith "no cap"


  let rec count_nodes mem =
    match mem with 
      | Leaf -> 0
      | Node (_,l,_,r) -> 1 + (count_nodes l) + (count_nodes r)
      | Cap (_,m) -> 1 + (count_nodes m)


end ;;






