

module type S = sig
  type state
  type t
  val empty : t
  val add_state : t -> state -> bool -> t
  val get : t -> state
  val move_back : t -> t
  val move_back_n : t -> int -> t
end


module Make (State : Gbc_core.State.S) : (S with type state = State.t) = struct
  type state = State.t
  type t = state list

  let empty = []
  let add_state xs st _ = st :: xs
  let get =
    function
    | st :: _ -> st
    | [] -> failwith "Can't move back."

  let move_back =
    function
    | [x] -> [x]
    | _ :: xs -> xs

  let rec move_back_n xs = function
  | 0 -> xs
  | n -> move_back_n (move_back xs) (n-1)

end
