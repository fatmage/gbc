module type S = sig
  type state

  val exec_dma : state -> int -> state * int
end

module MakeOAM (State : State.S) : S with type state = State.t = struct
  type state = State.t

  let exec_dma (st : state) mc =
    let rec aux (st : state) (DMAState.OAM.State.Active { src; progress }) n =
      match (progress, n) with
      | 160, _ ->
          ({ st with dma_oam = DMAState.OAM.set_state st.dma_oam Inactive }, 0)
      | progress, 0 ->
          ( {
              st with
              dma_oam =
                DMAState.OAM.set_state st.dma_oam @@ Active { src; progress };
            },
            0 )
      | progress, n ->
          aux
            (State.Bus.set8 st (0xFE00 + progress)
               (State.Bus.get8 st (src + progress)))
            (DMAState.OAM.State.Active { src; progress = progress + 1 })
            (n - 1)
    in
    match st.dma_oam.state with
    | Inactive -> (st, 0)
    | _ -> aux st st.dma_oam.state mc
end

module MakeVRAM (State : State.S) : S with type state = State.t = struct
  type state = State.t

  let exec_dma (st : state) _ =
    let in_progress = DMAState.VRAM.in_progress st.dma_vram in
    match (st.activity, in_progress) with
    | Halted _, _ | Stopped, _ -> (st, 0)
    | Running, false -> (st, 0)
    | Running, true -> (
        let rec transfer st src dst = function
          | 0 -> st
          | n ->
              transfer
                (State.Bus.set8 st dst (State.Bus.get8 st src))
                (src + 1) (dst + 1) (n - 1)
        in

        let length = DMAState.VRAM.length_real st.dma_vram in
        let src = DMAState.VRAM.source st.dma_vram in
        let dst = DMAState.VRAM.destination st.dma_vram in
        match DMAState.VRAM.mode st.dma_vram with
        | true ->
            (* General Purpose *)
            let st = transfer st src dst length in
            let mcyc = if State.get_speed st then length else length / 2 in
            ( {
                st with
                dma_vram =
                  { st.dma_vram with hdma5 = 0xFF; in_progress = false };
              },
              mcyc )
        | false -> (
            (* HBlank*)
            match State.GPUmem.get_mode st.gpu_mem with
            | HBlank (_, _) ->
                let st = transfer st src dst 0x10 in
                let mcyc = if State.get_speed st then 0x10 else 0x8 in
                let length = length - 0x10 in
                if length = 0 then
                  ( {
                      st with
                      dma_vram =
                        { st.dma_vram with hdma5 = 0xFF; in_progress = false };
                    },
                    mcyc )
                else
                  let hdma5 = (length / 0x10) - 1 in
                  let hdma1 = Utils.get_high src in
                  let hdma2 = Utils.get_low src in
                  let hdma3 = Utils.get_high dst in
                  let hdma4 = Utils.get_low dst in
                  ( {
                      st with
                      dma_vram =
                        {
                          hdma1;
                          hdma2;
                          hdma3;
                          hdma4;
                          hdma5;
                          in_progress = true;
                        };
                    },
                    mcyc )
            | _ -> (st, 0)))
end
