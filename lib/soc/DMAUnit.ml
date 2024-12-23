open State

module OAM = struct

  let exec_dma cpu_state mc =
    let rec aux cpu_state (DMAState.OAM.State.Active {src;progress}) n =
      match progress, n with
      | 160, _ -> { cpu_state with dma_oam = DMAState.OAM.set_state cpu_state.dma_oam Inactive }
      | m, 0   -> { cpu_state with dma_oam = DMAState.OAM.set_state cpu_state.dma_oam @@ Active {src; progress} }
      | m, n   -> aux (State.set_v8 cpu_state (0xFE00 + m) (State.get_v8 cpu_state (src + m))) (DMAState.OAM.State.Active {src; progress = m + 1}) (n - 1)
    in
    match cpu_state.dma_oam.state with
    | Inactive -> cpu_state
    | _        -> aux cpu_state cpu_state.dma_oam.state mc

end

(* move dma and hdma here *)

module VRAM = struct

  let exec_dma cpu_state mc = cpu_state

end
