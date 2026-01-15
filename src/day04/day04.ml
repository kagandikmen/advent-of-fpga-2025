(*
 *
 * AoF - Hardcaml Solution for Day 4
 * Created:     2025-12-22
 * Modified:    2026-01-15
 * Author:      Kagan Dikmen
 *
 *)

(*************************** IMPORTANT ****************************)
(* For an older solution, see src/old/day04/                      *)
(******************************************************************)

open! Core
open! Hardcaml
open! Hardcaml_aof
open! Hardcaml_arty
open! Signal

let u4 x = uresize x 4
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

let create_logic ~clock ~clear ~max_passes (uart_rx: Signal.t Uart.Byte_with_valid.t) =

  let max_row_dim = (1 lsl 8) in
  let max_col_dim = (1 lsl 8) in
  let mem_depth = max_row_dim * max_col_dim in
  let addr_width = (Math.ceil_log2 mem_depth) in

  let row_col_to_addr r c =
    let r' = uresize r addr_width in
    let c' = uresize c addr_width in
    let r'' = (Signal.sll r' 8) in
    r'' +: c'
  in

  let nb_offsets =
  [
    - max_col_dim - 1; 
    - max_col_dim;
    - max_col_dim + 1;
    - 1;
    + 1;
    + max_col_dim - 1;
    + max_col_dim;
    + max_col_dim + 1;
  ]
  in

  let is_at = (uart_rx.valid &: (uart_rx.value ==:. Char.to_int '@')) -- "is_at" in
  let is_dot = (uart_rx.valid &: (uart_rx.value ==:. Char.to_int '.')) -- "is_dot" in
  let is_cell = (is_at |: is_dot) -- "is_cell" in
  let is_stx = (uart_rx.valid &: (uart_rx.value ==:. 0x02)) -- "is_stx" in
  let is_etx = (uart_rx.valid &: (uart_rx.value ==:. 0x03)) -- "is_etx" in
  let is_nl = (uart_rx.valid &: (uart_rx.value ==:. Char.to_int '\n')) -- "is_nl" in

  let spec = Reg_spec.create
    ~clock
    ~clear
    ()
  in

  let row_load = reg_fb (Reg_spec.override spec ~clear_to:(one 8))
    ~enable:vdd
    ~width:8
    ~f:(fun prev ->
      mux2 (is_nl |: is_etx) (prev +:. 1) prev)
    -- "row_load"
  in

  let col_load = reg_fb (Reg_spec.override spec ~clear_to:(one 8))
    ~enable:vdd
    ~width:8
    ~f:(fun prev ->
      mux2 is_nl (one 8) (mux2 is_cell (prev +:. 1) prev))
    -- "col_load"
  in

  (* states of the FSM *)
  let phase_w = wire 3 in 
  let phase = reg spec phase_w -- "phase" in
  let p_idle = phase ==:. 0 in
  let p_load = phase ==:. 1 in
  let p_switch = phase ==:. 2 in  (* takes one cycle *)
  let p_remove = phase ==:. 3 in
  let p_done = phase ==:. 4 in

  let done_th = Signal.of_int ~width:8 100 in
  let done_ctr = reg_fb spec
    ~enable:vdd ~width:8 ~f:(fun prev -> mux2 p_done (prev +:. 1) prev)
  in

  (* we have dim+2 rows & cols in hw. the first and last ones are for the edges. so we reset to one. *)
  let row_remove_w = wire 8 in
  let row_remove = reg (Reg_spec.override spec ~clear_to:(one 8)) row_remove_w -- "row_remove" in 

  let col_remove_w = wire 8 in
  let col_remove = reg (Reg_spec.override spec ~clear_to:(one 8)) col_remove_w -- "col_remove" in

  let addr = mux2 p_load (row_col_to_addr row_load col_load) (mux2 p_remove (row_col_to_addr row_remove col_remove) (zero addr_width)) -- "addr" in
  
  let last_row = (row_remove +:. 1 ==: row_load) -- "last_row" in
  let last_col = (col_remove +:. 1 ==: col_load) -- "last_col" in
  let last_cell = (last_row &: last_col) -- "last_cell" in

  let p_idle_done = (p_idle &: is_stx) -- "p_idle_done" in
  let p_load_done = (p_load &: is_etx) -- "p_load_done" in
  let p_switch_done = p_switch in
  let p_remove_done = (p_remove &: last_cell) -- "p_remove_done" in
  let p_done_done = (p_done &: (done_ctr >=: done_th)) -- "p_done_done" in

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

  let is_next_load = p_idle_done in
  let is_next_remove = p_switch_done in
  let is_next_done = p_remove_done &: (((max_passes ==:. 0) &: pass_removed_zero) |: ((max_passes >:. 0) &: (max_passes ==: (remove_ctr +: one 8)))) in
  let is_next_switch_from_load = p_load_done in
  let is_next_switch_from_remove = p_remove_done &: (~:is_next_done) in
  let is_next_switch = is_next_switch_from_load |: is_next_switch_from_remove in
  let is_next_idle = p_done_done in

  let phase_next =
    mux2 clear (zero 3)
      (mux2 is_next_idle (Signal.of_int ~width:3 0)
        ((mux2 is_next_load (Signal.of_int ~width:3 1)
          (mux2 is_next_switch (Signal.of_int ~width:3 2)
            (mux2 is_next_remove (Signal.of_int ~width:3 3)
              (mux2 is_next_done (Signal.of_int ~width:3 4)
                phase))))))
    -- "phase_next"
  in

  assign phase_w phase_next;

  (* important distinction: field means cell that is not for the edge
  field = cell that really corresponds to a dot/at from the input text *)
  let go_back_to_field0 = clear |: (phase <>: phase_next) in

  let col_next_w =
    mux2 go_back_to_field0
      (one 8)
      (mux2 p_remove
        (mux2 last_col
          (one 8)
          (col_remove +:. 1))
        col_remove)
    -- "col_next_w"
  in

  let row_remove_next =
    mux2 go_back_to_field0
      (one 8)
      (mux2 p_remove
        (mux2 last_col
          (mux2 last_row
            (one 8)
            (row_remove +:. 1))
          row_remove)
        row_remove)
    -- "row_remove_next"
  in

  assign col_remove_w col_next_w;
  assign row_remove_w row_remove_next;

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

let create ~clock ~clear ~cycles_per_bit ~max_passes uart_rx_value =
  let uart_rx = Uart.Expert.create_rx_state_machine
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in
  create_logic ~clock ~clear ~max_passes uart_rx
;;