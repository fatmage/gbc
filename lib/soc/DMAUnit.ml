
module type S = sig
  module State : State.S
  val exec_dma : State.t -> int -> State.t
end

module MakeOAM (M : State.S) : S = struct
  module State = M

  let exec_dma (st : State.t) mc =
    let rec aux (st : State.t) (DMAState.OAM.State.Active {src;progress}) n =
      match progress, n with
      | 160, _ -> { st with dma_oam = DMAState.OAM.set_state st.dma_oam Inactive }
      | m, 0   -> { st with dma_oam = DMAState.OAM.set_state st.dma_oam @@ Active {src; progress} }
      | m, n   -> aux (State.set_v8 st (0xFE00 + m) (State.get_v8 st (src + m))) (DMAState.OAM.State.Active {src; progress = m + 1}) (n - 1)
    in
    match st.dma_oam.state with
    | Inactive -> st
    | _        -> aux st st.dma_oam.state mc

end

(* move dma and hdma here *)

module MakeVRAM (M : State.S) : S = struct
  module State = M

  let exec_dma (st : State.t) mc = st

end
