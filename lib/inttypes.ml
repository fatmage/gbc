


module type IntType = sig
  type t 

  val max_val : t 
  val zero : t
  val one : t  
  val two : t

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


let make_int_module : int -> (module IntType) = fun max_val -> (module struct
  type t = int
  
  let max_val = max_val
  let zero = 0
  let one = 1
  let two = 2

  let compare = compare
  let eq = fun a -> fun b -> a == b 
  let neq = fun a -> fun b -> a != b
  let gt = fun a -> fun b -> a > b
  let lt = fun a -> fun b -> a < b
  let gte = fun a -> fun b -> (eq a b) || (gt a b)
  let lte = fun a -> fun b -> (eq a b) || (lt a b)
  let logand = fun a -> fun b -> a land b 
  let logor = fun a -> fun b -> a lor b
  let logxor = fun a -> fun b -> a lxor b 
  let shr = fun a -> fun b -> Int.shift_right a b
  let shl = fun a -> fun b -> (Int.shift_left a b) land max_val
  let add = fun a -> fun b -> (a + b) land max_val
  let sub = fun a -> fun b -> let res = a - b in if res < 0 then (max_val + res + 1) else res
  let mul = fun a -> fun b -> (a * b) land max_val
  let div = fun a -> fun b -> (a / b) land max_val
  let rem = fun a -> fun b -> (a mod b) land max_val
  let succ = fun a -> add a one
  let pred = fun a -> sub a one

  let of_int = fun v -> v land max_val
  let to_int = fun v -> v

  let to_string = Int.to_string
  let to_hexstring = fun v -> Printf.sprintf "0x%x" v 
  let pp = fun fmt -> fun v -> Format.fprintf fmt "%s" (to_string v)
  let pp_hex = fun fmt -> fun v -> Format.fprintf fmt "%s" (to_hexstring v)
end : IntType
)

module U8 : IntType = struct
  module B = (val make_int_module 0xFF : IntType)
  include B
end


module U16 : IntType = struct
  module B = (val make_int_module 0xFFFF : IntType)
  include B
end

type uint8 = U8.t 
type uint16 = U16.t

let u8_to_16 : U8.t -> U16.t = fun u8 -> U16.of_int (U8.to_int u8) 
let u16_to_8 : U16.t -> U8.t = fun u16 -> U8.of_int (U16.to_int u16)



