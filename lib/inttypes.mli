module type IntType = sig
    type t 
  
    val max_val : t 
    val zero : t
    val one : t  
    val two: t
  
    val compare : t -> t -> int
    val eq : t -> t -> bool
    val neq : t -> t -> bool
    val gt : t -> t -> bool
    val gte : t -> t -> bool
    val lt : t -> t -> bool 
    val lte : t -> t -> bool
    val logand : t -> t -> t 
    val logor : t -> t -> t 
    val logxor : t -> t -> t 
    val shr : t -> t -> t 
    val shl : t -> t -> t 
    val add : t -> t -> t 
    val sub : t -> t -> t 
    val mul : t -> t -> t 
    val div : t -> t -> t 
    val rem : t -> t -> t 
    val succ : t -> t
    val pred : t -> t
  
    val of_int : int -> t
    val to_int : t -> int
  
    val to_string : t -> string 
    val to_hexstring : t -> string 
    val pp : Format.formatter -> t -> unit
    val pp_hex : Format.formatter -> t -> unit
  end

module U8 : IntType
module U16 : IntType

type uint8 = U8.t 
type uint16 = U16.t 


val u8_to_16 : U8.t -> U16.t 
val u16_to_8 : U16.t -> U8.t


