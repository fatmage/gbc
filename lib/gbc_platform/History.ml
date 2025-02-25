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

module Make (GBC : Gbc_core.CGB.S) : S with type state = GBC.State.t = struct
  type state = GBC.State.t
  type input_entry = { buttons : int; dpad : int; step : int }
  type input_history = input_entry list
  type entry = state * int * input_history
  type t = entry list * int

  let empty = ([], 0)
  let is_empty (_, n) = n = 0

  let mapfst_append f xs ys =
    let rec aux xs ys acc =
      match (xs, ys) with
      | [], [] -> List.rev acc
      | x :: xs, ys -> aux xs ys (f x :: acc)
      | [], y :: ys -> aux [] ys (y :: acc)
    in
    aux xs ys []

  let add_state xs st vblank =
    let history, length =
      match xs with
      | [], 0 -> ([ (st, 0, []) ], 1)
      | (pst, n, is) :: states, l -> (
          match vblank with
          | true -> ((st, 0, []) :: (pst, n, is) :: states, l + 1)
          | false -> (
              match is with
              | [] ->
                  let buttons, dpad = GBC.State.get_joypad st in
                  let jp_changed = GBC.State.joypad_diff pst buttons dpad in
                  if jp_changed then
                    ( (pst, n + 1, { buttons; dpad; step = n } :: is) :: states,
                      l )
                  else ((pst, n + 1, is) :: states, l)
              | { buttons; dpad; _ } :: xs ->
                  let jp_changed = GBC.State.joypad_diff st buttons dpad in
                  if jp_changed then
                    let buttons, dpad = GBC.State.get_joypad st in
                    ( (pst, n + 1, { buttons; dpad; step = n } :: is) :: states,
                      l )
                  else ((pst, n + 1, is) :: states, l)))
    in
    match length with
    | 36000 ->
        let rec coalesce history acc =
          match history with
          | (st1, n1, is1)
            :: (st2, n2, is2)
            :: (st3, n3, is3)
            :: (st4, n4, is4)
            :: history ->
              let update_inputs offset { buttons; dpad; step } =
                { buttons; dpad; step = step + offset }
              in
              let is' = mapfst_append (update_inputs @@ (n4 + 1)) is3 is4 in
              let is' =
                mapfst_append (update_inputs @@ (n4 + n3 + 2)) is2 is'
              in
              let is' =
                mapfst_append (update_inputs @@ (n4 + n3 + n2 + 3)) is1 is'
              in
              let new_entry = (st4, n4 + n3 + n2 + n1, is') in
              coalesce history @@ (new_entry :: acc)
          | _ -> (List.rev acc, 9000)
        in
        coalesce history []
    | _ -> (history, length)

  let rec replay st n is_old is_new i =
    match i with
    | _ when i >= n -> (st, is_new)
    | _ -> (
        let st, _, _ = GBC.cpu_step st in
        match is_old with
        | [] -> replay st n is_old is_new (i + 1)
        | { buttons; dpad; step } :: is ->
            if step = i then
              let st = GBC.State.set_joypad st buttons dpad in
              replay st n is ({ buttons; dpad; step } :: is_new) (i + 1)
            else replay st n is_old is_new (i + 1))

  let move_back = function
    | [ (st, 0, is) ], l -> (st, ([ (st, 0, is) ], l))
    | [ (st, n, is) ], l ->
        let is_chrono = List.rev is in
        let st', new_is = replay st (n - 1) is_chrono [] 0 in
        (st', ([ (st, n - 1, new_is) ], l))
    | (_, 0, _) :: (st2, n2, is2) :: xs, l ->
        let is2_chrono = List.rev is2 in
        let st, _ = replay st2 n2 is2_chrono [] 0 in
        (st, ((st2, n2, is2) :: xs, l - 1))
    | (st1, n1, is1) :: (st2, n2, is2) :: xs, l ->
        let is2_chrono = List.rev is2 in
        let _, _ = replay st2 (n2 + 1) is2_chrono [] 0 in
        let is1_chrono = List.rev is1 in
        let st, new_is1 = replay st1 (n1 - 1) is1_chrono [] 0 in
        (st, ((st1, n1 - 1, new_is1) :: (st2, n2, is2) :: xs, l))

  let move_back_frame = function
    | [ (st, _, _) ], 1 | [ (_, 0, _); (st, _, _) ], 2 ->
        (st, ([ (st, 0, []) ], 1))
    | (_, 0, _) :: (st2, _, _) :: (st3, n3, is3) :: xs, l ->
        let is3_chrono = List.rev is3 in
        let _, _ = replay st3 (n3 + 1) is3_chrono [] 0 in
        (st2, ((st2, 0, []) :: (st3, n3, is3) :: xs, l - 1))
    | (st1, _, _) :: (st2, n2, is2) :: xs, l ->
        let is2_chrono = List.rev is2 in
        let _, _ = replay st2 (n2 + 1) is2_chrono [] 0 in
        (st1, ((st1, 0, []) :: (st2, n2, is2) :: xs, l))

  let move_back_second history =
    let rec aux history i =
      match (history, i) with
      | ([ (st, _, _) ], l), _ -> (st, ([ (st, 0, []) ], l))
      | history, 0 -> move_back_frame history
      | (_ :: history, l), i -> aux (history, l - 1) (i - 1)
    in
    aux history 59
end

module MakeDense (GBC : Gbc_core.CGB.S) : S with type state = GBC.State.t =
struct
  type state = GBC.State.t
  type t = state list

  let empty = []
  let is_empty = List.is_empty
  let add_state xs st _ = st :: xs

  let move_back = function
    | [] -> (GBC.State.initial, [])
    | [ x ] -> (x, [ x ])
    | x :: xs -> (x, xs)

  let move_back_frame = move_back
  let move_back_second = move_back
end

module MakeNone (GBC : Gbc_core.CGB.S) : S with type state = GBC.State.t =
struct
  type state = GBC.State.t
  type t = state list

  let cnt = ref 0
  let empty = []
  let is_empty = List.is_empty

  let add_state xs _ vblank =
    if vblank then (
      cnt := !cnt + 1;
      print_endline @@ string_of_int !cnt);
    xs

  let move_back _ = (GBC.State.initial, [])
  let move_back_frame = move_back
  let move_back_second = move_back
end
