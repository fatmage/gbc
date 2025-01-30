module OAM = struct
  module State = struct
    type t = Inactive | Active of { src : int; progress : int }

    let initial = Inactive
  end

  type t = { dma : int; state : State.t }

  let initial = { dma = 0; state = State.initial }
  let get m _ = m.dma

  let set m _ v =
    match m with
    | { state = Inactive; _ } ->
        { dma = v; state = Active { src = v lsl 8; progress = 0 } }
    | { state; _ } -> { dma = v; state }

  let in_range i = i = 0xFF46
  let state { state; _ } = state
  let set_state m v = { m with state = v }
end

(* move dma and hdma here *)

module VRAM = struct
  type t = {
    hdma1 : int;
    hdma2 : int;
    hdma3 : int;
    hdma4 : int;
    hdma5 : int;
    in_progress : bool;
  }

  let initial =
    {
      hdma1 = 0xFF;
      hdma2 = 0xFF;
      hdma3 = 0xFF;
      hdma4 = 0xFF;
      hdma5 = 0xFF;
      in_progress = false;
    }

  let get m = function
    | 0xFF51 -> m.hdma1
    | 0xFF52 -> m.hdma2
    | 0xFF53 -> m.hdma3
    | 0xFF54 -> m.hdma4
    | 0xFF55 -> m.hdma5 land 0x7F lor if m.in_progress then 0 else 0x80
    | _ -> assert false

  let set m i v =
    match i with
    | 0xFF51 -> { m with hdma1 = v }
    | 0xFF52 -> { m with hdma2 = v land 0xF0 }
    | 0xFF53 -> { m with hdma3 = v land 0x1F }
    | 0xFF54 -> { m with hdma4 = v land 0xF0 }
    | 0xFF55 -> (
        match m.in_progress with
        | false -> { m with hdma5 = v; in_progress = true }
        | true ->
            if v land 0x80 = 0 then
              { m with hdma5 = 0x80 lor m.hdma5; in_progress = false }
            else { m with hdma5 = v })
    | _ -> assert false

  (* true - general purpose, false - HBlank *)
  let mode m = m.hdma5 land 0x80 > 0
  let length m = m.hdma5 land 0x7F
  let length_real m = (length m + 1) * 0x10
  let source m = (m.hdma1 lsl 8) lor m.hdma2
  let destination m = (m.hdma3 lsl 8) lor m.hdma4
  let in_range i = 0xFF51 <= i && i <= 0xFF55
  let in_progress m = m.in_progress
end
