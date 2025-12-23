(*
 *
 * AoF - Hardcaml Solution for Day 4 (Step 1 & Step 2)
 * Created:     2025-12-22
 * Modified:    2025-12-23
 * Author:      Kagan Dikmen
 *
 *)

(* TODO: Add FPGA build and RTL generation logic *)

open! Core
open! Hardcaml
open! Hardcaml_arty
open! Signal

let mul140 x = (sll x 7) +: (sll x 3) +: (sll x 2)
let add_offset addr off = if off >= 0 then addr +:. off else addr -:. (-off)

module Cell = struct
  type 'a t =
  {
    is_roll: 'a;
    is_accessible: 'a;
  }

  let pack (x: Signal.t t) : Signal.t = 
    concat_msb [ x.is_accessible; x.is_roll; ]

  let unpack (x: Signal.t) : Signal.t t =
    {
      is_roll = select x 0 0;
      is_accessible = select x 1 1;
    }
end

let create_day04_logic ~clock ~clear ~cycles_per_bit ~max_passes uart_rx_value =
  let dim = 138 in
  let dim' = dim + 2 in
  let mem_depth = dim' * dim' in
  let addr_width = 15 in

  let row_col_to_addr r c =
    let r' = uresize r addr_width in
    let c' = uresize c addr_width in
    let r140 = mul140 r' in
    r140 +: c'
  in

  let nb_offsets =
    [
      - dim' - 1; 
      - dim';
      - dim' + 1;
      - 1;
      + 1;
      + dim' - 1;
      + dim';
      + dim' + 1;
    ]
  in

  let uart_rx = Uart.Expert.create_rx_state_machine
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in

  let _uart_value = uart_rx.value -- "uart_value" in
  let _uart_valid = uart_rx.valid -- "uart_valid" in

  let is_at = (uart_rx.valid &: (uart_rx.value ==:. Char.to_int '@')) -- "is_at" in
  let is_dot = (uart_rx.valid &: (uart_rx.value ==:. Char.to_int '.')) -- "is_dot" in
  let is_cell = (is_at |: is_dot) -- "is_cell" in

  let spec = Reg_spec.create
    ~clock
    ~clear
    ()
  in

  (* states of the FSM *)
  let phase_w = wire 2 in 
  let phase = reg spec phase_w -- "phase" in
  let p_load = phase ==:. 0 in
  let p_mark = phase ==:. 1 in
  let p_remove = phase ==:. 2 in
  let p_done = phase ==:. 3 in

  (* we have dim+2 rows & cols in hw. the first and last ones are for the edges. so we reset to one. *)
  let row_w = wire 8 in
  let row = reg (Reg_spec.override spec ~clear_to:(one 8)) row_w -- "row" in 

  let col_w = wire 8 in
  let col = reg (Reg_spec.override spec ~clear_to:(one 8)) col_w -- "col" in

  let addr = (row_col_to_addr row col) -- "addr" in
  
  let last_row = row ==:. dim in
  let last_col = col ==:. dim in
  let last_cell = (last_row &: last_col) -- "last_cell" in

  let step_en = ((p_load &: is_cell) |: p_mark |: p_remove) -- "step_en" in 

  let p_load_done = (p_load &: is_cell &: last_cell) -- "p_load_done" in
  let p_mark_done = (p_mark &: last_cell) -- "p_mark_done" in
  let p_remove_done = (p_remove &: last_cell) -- "p_remove_done" in

  (* important: remove_ctr shows how many removes are COMPLETED *)
  let remove_ctr = reg_fb spec
    ~enable:vdd
    ~width:8
    ~f:(fun prev ->
      mux2 clear (zero 8) (mux2 p_remove_done (prev +:. 1) prev))
    -- "remove_ctr"
  in

  (* nr of removable cells found in mark state *)
  let latest_pass_removed_ctr_w = wire 16 in
  let latest_pass_removed_ctr = reg spec latest_pass_removed_ctr_w -- "latest_pass_removed_ctr" in
  let latest_pass_removed_zero = (latest_pass_removed_ctr ==:. 0) -- "latest_pass_removed_zero" in

  let is_next_remove = p_mark_done in
  let is_next_done = p_remove_done &: (((max_passes ==:. 0) &: latest_pass_removed_zero) |: ((max_passes >:. 0) &: (max_passes ==: (remove_ctr +: one 8)))) in
  let is_next_mark_from_load = p_load_done in
  let is_next_mark_from_remove = p_remove_done &: (~:is_next_done) in
  let is_next_mark = is_next_mark_from_load |: is_next_mark_from_remove in

  let phase_next =
    mux2 clear (zero 2)
      (mux2 is_next_mark (one 2)
        (mux2 is_next_remove (Signal.of_int ~width:2 2)
          (mux2 is_next_done (Signal.of_int ~width:2 3)
              phase)))
    -- "phase_next"
  in

  assign phase_w phase_next;

  (* important distinction: field means cell that is not for the edge
  field = cell that really corresponds to a dot/at from the input text *)
  let go_back_to_field0 = clear |: (phase <>: phase_next) in

  let col_next =
    mux2 go_back_to_field0
      (one 8)
      (mux2 step_en
        (mux2 last_col
          (one 8)
          (col +:. 1))
        col)
    -- "col_next"
  in

  let row_next =
    mux2 go_back_to_field0
      (one 8)
      (mux2 step_en
        (mux2 last_col
          (mux2 last_row
            (one 8)
            (row +:. 1))
          row)
        row)
    -- "row_next"
  in

  assign col_w col_next;
  assign row_w row_next;

  (* cell memory interface *)

  let cell_we_w = wire 1 in
  let cell_wdata_w = wire 2 in

  let cell_mem_write_port : Signal.write_port =
    {
      write_clock = clock;
      write_address = addr;
      write_enable = cell_we_w;
      write_data = cell_wdata_w;
    }
  in

  let nb_addrs =
    let nbs = List.map nb_offsets ~f:(fun off -> add_offset addr off) in
    Array.of_list (addr :: nbs)
  in

  let cell_mem = Signal.multiport_memory mem_depth
    ~write_ports: [| cell_mem_write_port |]
    ~read_addresses: nb_addrs
  in

  let current_cell = Cell.unpack cell_mem.(0) in
  let is_roll_current_cell = current_cell.is_roll -- "is_roll_current_cell" in
  let is_accessible_current_cell = current_cell.is_accessible -- "is_accessible_current_cell" in
  
  let nb_cells = Array.init 8 ~f:(fun i -> Cell.unpack cell_mem.(i+1)) in
  let num_nb_rolls = Array.fold nb_cells ~init:(zero 4) ~f:(fun acc c -> acc +: uresize c.is_roll 4) -- "num_nb_rolls" in

  let remove_en = (p_remove &: is_roll_current_cell &: is_accessible_current_cell) -- "remove_en" in

  let load_we = (p_load &: is_cell) -- "load_we" in
  let load_wdata = (Cell.pack { is_roll = is_at; is_accessible = zero 1; }) -- "load_wdata" in

  let mark_we = (num_nb_rolls <:. 4) -- "mark_we" in
  let mark_wdata = (Cell.pack { is_roll = is_roll_current_cell; is_accessible = (num_nb_rolls <:. 4); }) -- "mark_wdata" in
  let remove_wdata = Cell.pack { is_roll = gnd; is_accessible = zero 1; } in 
  (* does not matter that p_remove zeroes out is_accessible, it is recomputed during the following p_mark *)

  assign cell_we_w (mux2 p_load load_we (mux2 p_mark mark_we (mux2 p_remove remove_en gnd)));
  assign cell_wdata_w (mux2 p_load load_wdata (mux2 p_mark mark_wdata (mux2 p_remove remove_wdata (zero 2))));

  let latest_pass_removed_ctr_next =
    mux2 clear
      (zero 16)
      (mux2 is_next_mark
        (zero 16)
        (mux2 (p_remove &: remove_en)
          (latest_pass_removed_ctr +:. 1)
          latest_pass_removed_ctr))
  in

  assign latest_pass_removed_ctr_w latest_pass_removed_ctr_next;

  let total_removed = reg_fb spec
    ~enable:vdd
    ~width:16
    ~f:(fun prev -> 
      mux2 clear
        (zero 16)
        (mux2 remove_en
          (prev +:. 1)
          prev))
    -- "total_removed"
  in

  total_removed, p_done
;;