

module OAM = struct
  type dma_state = Inactive | Active of int

  type t = dma_state * int

let rec exec_dma cpu_state dma_state =
  let exec_dma_aux cpu_state =
    function
    | Inactive, old_dma ->
      let curr_dma = State.get_v8 cpu_state 0xFF46 in
      if old_dma != curr_dma then cpu_state, (Active 0, curr_dma) else cpu_state, (Inactive, old_dma)
    | Active 0, addr_start -> cpu_state, (Inactive, addr_start)
    | Active n, addr_start -> failwith "TODO"

  in
  function
  | 0 -> cpu_state, dma_state
  | n -> let c, d = exec_dma_aux cpu_state dma_state in exec_dma c d (n-1)
end


module HDMA = struct
  type hdma_state = Inactive | Active of int

  type t = hdma_state * int

let exec_dma cpu_state hdma_state mc =
  cpu_state, hdma_state

end
