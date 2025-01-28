

module type S = sig
  type state
  type t
  val empty : t
  val add_state : t -> state -> bool -> t
  val get : t -> state
  val move_back : t -> state * t
  val move_back_frame : t -> state * t
end

module Make (GBC : Gbc_core.CPU.S) : (S with type state = GBC.State.t) = struct
  type state = GBC.State.t

  type input_entry =  { joypad : int; step : int }
  type input_history = input_entry list

  type entry = state * int * input_history

  type t = entry list

  let empty = []

  let add_state xs st vblank =

    let r = match xs with
    | []                     -> [(st, 0, [])]
    | (pst, n, is) :: states ->
      match vblank with
      | true  -> (st, 0, []) :: xs
      | false ->
        let old_joypad = GBC.State.get_joypad pst in
        let joypad = GBC.State.get_joypad st  in
        if old_joypad = joypad then
          (pst, n + 1, is) :: states
        else
          (pst, n + 1, { joypad; step = n } :: is) :: states
        in
        print_endline @@ string_of_int @@ List.length r;
        r

  let get =
    function
    | (st, _, _) :: _ -> st
    | [] -> failwith "No state in history."


  let rec replay st n is_old is_new i =
    match i with
    | _ when i >= n -> st, is_new
    | _ ->
      let st, _, vblank = GBC.cpu_step st in
      match is_old with
      | [] ->
        replay st n is_old is_new (i+1)
      | { joypad; step } :: is ->
        if step = i then
          let st = GBC.State.set_joypad st joypad in
          replay st n is ({joypad;step} ::is_new) (i+1)
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

end

module MakeSimple (GBC : Gbc_core.CPU.S) : (S with type state = GBC.State.t) = struct

  type state = GBC.State.t
  type t = state list

  let empty = []
  let add_state xs st _ = st :: xs
  let get =
    function
    | st :: _ -> st
    | [] -> failwith "Can't move back."

  let move_back =
    function
    | [x] -> x, [x]
    | x :: xs -> x, xs

  let move_back_frame = move_back

end
