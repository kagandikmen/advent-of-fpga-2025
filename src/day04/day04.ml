(*
 *
 * AoF - Hardcaml Solution for Day 4 (NEW)
 * Created:     2025-12-22
 * Modified:    2026-01-12
 * Author:      Kagan Dikmen
 *
 *)

(* TODO: Add FPGA build and RTL generation logic *)

open! Core
open! Hardcaml
open! Hardcaml_aof
open! Hardcaml_arty
open! Signal

let u4 x = uresize x 4
let mul140 x = (sll x 7) +: (sll x 3) +: (sll x 2)
let add_offset addr off = if off >= 0 then addr +:. off else addr -:. (-off)

module Cell = struct
  type 'a t =
  {
    is_roll: 'a;
    removed_this_pass: 'a;
  }

  let pack (x: Signal.t t) : Signal.t = 
    concat_msb [ x.removed_this_pass; x.is_roll; ]

  let unpack (x: Signal.t) : Signal.t t =
    {
      is_roll = select x 0 0;
      removed_this_pass = select x 1 1;
    }
end

let create ~clock ~clear ~cycles_per_bit ~max_passes uart_rx_value =
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
  let p_switch = phase ==:. 1 in  (* takes one cycle *)
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

  let step_en = ((p_load &: is_cell) |: p_remove) -- "step_en" in 

  let p_load_done = (p_load &: is_cell &: last_cell) -- "p_load_done" in
  let p_switch_done = p_switch in
  let p_remove_done = (p_remove &: last_cell) -- "p_remove_done" in

  (* important: remove_ctr shows how many remove passes are COMPLETED *)
  let remove_ctr = reg_fb spec
    ~enable:vdd
    ~width:8
    ~f:(fun prev ->
      mux2 clear (zero 8) (mux2 p_remove_done (prev +:. 1) prev))
    -- "remove_ctr"
  in

  (* nr of removable cells found in remove state *)
  let pass_removed_ctr_w = wire 16 in
  let pass_removed_ctr = reg spec pass_removed_ctr_w -- "pass_removed_ctr" in
  let pass_removed_zero = (pass_removed_ctr ==:. 0) -- "pass_removed_zero" in

  let is_next_remove = p_switch_done in
  let is_next_done = p_remove_done &: (((max_passes ==:. 0) &: pass_removed_zero) |: ((max_passes >:. 0) &: (max_passes ==: (remove_ctr +: one 8)))) in
  let is_next_switch_from_load = p_load_done in
  let is_next_switch_from_remove = p_remove_done &: (~:is_next_done) in
  let is_next_switch = is_next_switch_from_load |: is_next_switch_from_remove in

  let phase_next =
    mux2 clear (zero 2)
      (mux2 is_next_switch (one 2)
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
  let nw_cell = Cell.unpack cell_mem.(1) in
  let n_cell = Cell.unpack cell_mem.(2) in
  let ne_cell = Cell.unpack cell_mem.(3) in
  let e_cell = Cell.unpack cell_mem.(4) in

  let is_roll_current_cell = current_cell.is_roll -- "is_roll_current_cell" in
  let nw_cell_removed = nw_cell.removed_this_pass in
  let n_cell_removed = n_cell.removed_this_pass in
  let ne_cell_removed = ne_cell.removed_this_pass in
  let e_cell_removed = e_cell.removed_this_pass in
  
  let nb_cells = Array.init 8 ~f:(fun i -> Cell.unpack cell_mem.(i+1)) in
  let num_nb_rolls_raw = Array.fold nb_cells ~init:(zero 4) ~f:(fun acc c -> acc +: uresize c.is_roll 4) -- "num_nb_rolls" in
  let num_nb_rolls =
    (num_nb_rolls_raw 
      +: u4 (nw_cell_removed ==:. 1)
      +: u4 (n_cell_removed ==:. 1)
      +: u4 (ne_cell_removed ==:. 1)
      +: u4 (e_cell_removed ==:. 1))
    -- "num_nb_rolls" 
  in

  let load_we = (p_load &: is_cell) -- "load_we" in
  let load_wdata = (Cell.pack { is_roll = is_at; removed_this_pass = zero 1; }) -- "load_wdata" in

  let is_accessible_current_cell = (num_nb_rolls <:. 4) -- "is_accessible_current_cell" in
  let is_current_cell_removed_last_pass = current_cell.removed_this_pass in

  let remove_en = p_remove &: ((is_roll_current_cell &: is_accessible_current_cell) |: is_current_cell_removed_last_pass) -- "remove_en" in
  let remove_wdata = Cell.pack { is_roll = gnd; removed_this_pass = (is_roll_current_cell &: is_accessible_current_cell); } in 

  assign cell_we_w (mux2 p_load load_we (mux2 p_remove remove_en gnd));
  assign cell_wdata_w (mux2 p_load load_wdata (mux2 p_remove remove_wdata (zero 2)));

  let pass_removed_ctr_next =
    mux2 clear
      (zero 16)
      (mux2 is_next_switch
        (zero 16)
        (mux2 (p_remove &: remove_en)
          (pass_removed_ctr +:. 1)
          pass_removed_ctr))
  in

  assign pass_removed_ctr_w pass_removed_ctr_next;

  let total_removed = reg_fb spec
    ~enable:vdd
    ~width:16
    ~f:(fun prev -> 
      mux2 clear
        (zero 16)
        (mux2 (remove_en &: (~:is_current_cell_removed_last_pass))
          (prev +:. 1)
          prev))
    -- "total_removed"
  in

  (* waveform stuff *)
  let uart_value = uart_rx.value -- "uart_value" in
  let uart_valid = uart_rx.valid -- "uart_valid" in

  let debug_output =
    concat_msb
      [
        uart_value;
        uart_valid;
      ];
  in

  total_removed, p_done, debug_output
;;