module S : Addressable.S = struct
  type t = int (* u8 *) list
  let empty = []
  let get t i = List.nth t i
  let set t i v = t
  let in_range i = true
end