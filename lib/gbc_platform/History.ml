

module type S = sig
  type state
  type t
  val empty : t
  val add_state : t -> state -> bool -> t
  val move_back : t -> state * t
  val move_back_frame : t -> state * t
  val move_back_second : t -> state * t
  val is_empty : t -> bool
end

module Make (GBC : Gbc_core.CGB.S) : (S with type state = GBC.State.t) = struct

  type state = GBC.State.t
  type input_entry =  { buttons : int; dpad : int; step : int }
  type input_history = input_entry list
  type entry = state * int * input_history
  type t = entry list

  let empty = []

  let is_empty = List.is_empty

  let add_state xs st vblank =
    match xs with
    | []                     -> [(st, 0, [])]
    | (pst, n, is) :: states ->
      match vblank with
      | true  -> (st, 0, []) :: xs
      | false ->
        let buttons, dpad = GBC.State.get_joypad st in
        let jp_changed = GBC.State.joypad_diff pst buttons dpad in
        if jp_changed then
          (pst, n + 1, { buttons; dpad; step = n } :: is) :: states
        else
          (pst, n + 1, is) :: states

  let rec replay st n is_old is_new i =
    match i with
    | _ when i >= n -> st, is_new
    | _ ->
      let st, _, vblank = GBC.cpu_step st in
      match is_old with
      | [] ->
        replay st n is_old is_new (i+1)
      | { buttons; dpad; step } :: is ->
        if step = i then
          let st = GBC.State.set_joypad st buttons dpad in
          replay st n is ({buttons;dpad;step} ::is_new) (i+1)
        else
          replay st n is_old is_new (i+1)

  let move_back =
    function
    | [(st, 0, is)] -> st, [(st, 0, is)]
    | [(st, n, is)] ->
      let is_chrono = List.rev is in
      let st', new_is = replay st (n-1) is [] 0 in
      st', [(st, n-1, new_is)]
    | (st1, 0, is1) :: (st2, n2, is2) :: xs ->
      let is2_chrono = List.rev is2 in
      let st, _ = replay st2 n2 is2 [] 0 in
      st, (st2, n2, is2) :: xs
    | (st1, n1, is1) :: (st2, n2, is2) :: xs ->
      let is2_chrono = List.rev is2 in
      let st1', _ = replay st2 (n2+1) is2 [] 0 in
      let is1_chrono = List.rev is1 in
      let st, new_is1 = replay st1 (n1-1) is1 [] 0 in
      st, (st1, n1-1, new_is1) :: (st2, n2, is2) :: xs

  let move_back_frame =
    function
    | [st, n, is] | [(_, 0, _); (st, n, is)] -> st, [st, 0, []]
    | (st1, 0, is1) :: (st2, n2, is2) :: (st3, n3, is3) :: xs ->
      let is3_chrono = List.rev is3 in
      let _, _ = replay st3 (n3+1) is3 [] 0 in
      st2, (st2, 0, []) :: (st3, n3, is3) :: xs
    | (st1, n1, is1) :: (st2, n2, is2) :: xs ->
      let is2_chrono = List.rev is2 in
      let _, _ = replay st2 (n2+1) is2 [] 0 in
      st1, (st1, 0, []) :: (st2, n2, is2) :: xs

  let move_back_second history =
    let rec aux history i =
      match history, i with
      | [(st,n,is)], _  -> st, [(st, 0, [])]
      | history, 0      -> move_back_frame history
      | _ :: history, i -> aux history (i-1)
    in
    aux history 59

end

module MakeDense (GBC : Gbc_core.CGB.S) : (S with type state = GBC.State.t) = struct

  type state = GBC.State.t
  type t = state list

  let empty = []
  let is_empty = List.is_empty

  let add_state xs st _ = st :: xs

  let move_back =
    function
    | []  -> GBC.State.initial, []
    | [x] -> x, [x]
    | x :: xs -> x, xs

  let move_back_frame = move_back
  let move_back_second = move_back

end
