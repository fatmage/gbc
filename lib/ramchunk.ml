open Inttypes


(* 4rd version - bytes in nodes with implicit inorder indexing *)

module S : Addressable.S = struct

  type t = Leaf | Node of int * t * uint8 * t | Cap of uint8 * t

  let rec count_nodes mem =
    match mem with
      | Leaf -> 0
      | Node (_,l,_,r) -> 1 + (count_nodes l) + (count_nodes r)
      | Cap (_,m) -> 1 + (count_nodes m)
  let empty =
    let rec help n i offset=
      match n with
        | 0 -> Leaf
        | n -> Node (i + offset, help (n-1) (i/2) offset, U8.zero,
                                 help (n-1) (i/2) (offset + i))
    in
    Cap (U8.zero, help 13 4096 0)


  let rec get mem index =
    match mem, index with
      | Cap (v,_), 0 -> v
      | Cap (_,m), _ -> get m index
      | Node (i,l,v,r), index ->  if index == i then
                                    v else
                                    if index < i then
                                      get l index else
                                      get r index
      | _,_ -> failwith "no cap"

  let rec set mem index v =
    match mem, index with
      | Cap (_,m), 0 -> Cap (v,m)
      | Cap (x,m), _ -> Cap (x, set m index v)
      | Node (i,l,x,r), index ->  if index == i then
                                    Node (i,l,v,r) else
                                    if index < i then
                                      Node (i,set l index v,x,r) else
                                      Node (i,l,x,set r index v)
      | _,_ -> failwith "no cap"


  let in_range n = if (n < 0 && n >= 8192) then true else false

end ;;
