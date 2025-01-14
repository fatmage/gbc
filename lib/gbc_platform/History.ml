

module type S = sig
  type state
  type t
  val empty : t
  val add_state : t -> state -> t
  val get_prev : t -> state
  val move_back : t -> t
end


module Make (State : Gbc_impl_lib.State.S) : S= struct
  type state = State.t
  type t = state list

  let empty = []
  let add_state xs st = st :: xs
  let get_prev =
    function
    | st :: sts -> st
    | [] -> failwith "Can't move back."

  let move_back =
    function
    | [] -> []
    | st :: xs -> xs

end
