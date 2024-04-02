open Inttypes


module type MemoryChunk = sig
  
    type mem
    val empty : mem 
    val get : int -> mem -> uint8 
    val set : uint8 -> int -> mem -> mem 
  
end


module Mem : MemoryChunk