module OAM = struct

  type t = Inactive | Active of { src : int; progress : int }

  let initial = Inactive


end

(* move dma and hdma here *)

module VRAM = struct

  module State = struct
    type t = Inactive | Active of { src : int; dst : int; progress : int; elapsed : int; length : int }
    let initial = Inactive
  end

  type t = { hdma1 : int; hdma2 : int; hdma3 : int; hdma4 : int; hdma5 : int; state : State.t }
  let initial = { hdma1 = 0; hdma2 = 0; hdma3 = 0; hdma4 = 0; hdma5 = 0; state = State.initial }
  let get m =
    function
    | 0xFF51 -> m.hdma1
    | 0xFF52 -> m.hdma2
    | 0xFF53 -> m.hdma3
    | 0xFF54 -> m.hdma4
    | 0xFF55 -> m.hdma5

  let set m i v =
    match i with
    | 0xFF51 -> { m with hdma1 = v }
    | 0xFF52 -> { m with hdma2 = v }
    | 0xFF53 -> { m with hdma3 = v }
    | 0xFF54 -> { m with hdma4 = v }
    | 0xFF55 -> { m with hdma5 = v }

    let in_range i = 0xFF51 <= i && i <= 0xF55
end