
module type S = sig
  type state
  val exec_dma : state -> int -> state
end

module MakeOAM (State : State.S) : (S with type state = State.t) = struct
  type state = State.t

  let exec_dma (st : state) mc =
    let rec aux (st : state) (DMAState.OAM.State.Active {src;progress}) n =
      match progress, n with
      | 160, _ -> { st with dma_oam = DMAState.OAM.set_state st.dma_oam Inactive }
      | progress, 0   -> { st with dma_oam = DMAState.OAM.set_state st.dma_oam @@ Active {src; progress} }
      | progress, n   -> Utils.print_hex "Dma at" (0xFE00 + progress); aux (State.set_v8 st (0xFE00 + progress) (State.get_v8 st (src + progress))) (DMAState.OAM.State.Active {src; progress = progress + 1}) (n - 1)
    in
    match st.dma_oam.state with
    | Inactive -> st
    | _        -> aux st st.dma_oam.state mc

end

(* TODO: HDMA *)

module MakeVRAM (State : State.S) : (S with type state = State.t) = struct
  type state = State.t

  let exec_dma (st : state) _ = st

end
