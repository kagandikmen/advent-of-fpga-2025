(*
 *
 * AoF - Hardcaml Solution for Day 1 (Step 1 & Step 2)
 * Created:     2025-12-12
 * Modified:    2025-12-28
 * Author:      Kagan Dikmen
 *
 *)

(* TODO: Add FPGA build and RTL generation logic *)

open! Core
open! Hardcaml
open! Hardcaml_aof
open! Hardcaml_arty
open! Signal

let divmod100 (d : Signal.t) : Signal.t * Signal.t =
  let step (q, r) =
    let is_ge100 = r >=:. 100 in
    let q' = mux2 is_ge100 (q +:. 1) q in
    let r' = mux2 is_ge100 (r -:. 100) r in
    q', r'
  in
  
  List.fold (List.init 9 ~f:Fn.id)
    ~init:(zero (width d), d)
    ~f:(fun acc _ -> step acc)
;;

let create_counting_logic ~clock ~clear ~cycles_per_bit uart_rx_value =
  
  (* UART receiver state machine *)
  let uart_rx = Uart.Expert.create_rx_state_machine
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in

  (* for debugging *)
  let _uart_value = uart_rx.value -- "uart_value" in
  let _uart_valid = uart_rx.valid -- "uart_valid" in

  let spec = Reg_spec.create 
    ~clock
    ()
  in

  let is_upcoming_msb = reg_fb spec
    ~enable:vdd
    ~width:1
    ~f:(fun prev ->
      mux2 uart_rx.valid (~:prev) prev)
    -- "is_upcoming_msb"
  in

  let lsb_reg = reg spec
    ~enable:(uart_rx.valid &: (~:is_upcoming_msb))
    uart_rx.value
    -- "lsb_reg"
  in

  let msb_arrived = (uart_rx.valid &: is_upcoming_msb) -- "msb_arrived" in

  let value_16 = concat_msb [ uart_rx.value; lsb_reg; ] -- "value_16" in  (* this is a "sliding window" *)
  let value_32 = sresize value_16 32 -- "value_32" in

  let is_dir_left = msb value_32 -- "is_dir_left" in
  let turn_mag_32 = mux2 is_dir_left (negate value_32) value_32 -- "turn_mag_32" in

  let next_pos_wire = wire 32 in
  
  let pos_reg = reg_fb spec
    ~enable:vdd
    ~width:32 
    ~f:(fun _prev ->
      let init_value = of_int ~width:32 50 in
      mux2 clear init_value next_pos_wire)
    -- "pos_reg" 
  in

  let wrap_eff, turn_eff = divmod100 turn_mag_32 in
  
  let turn_knob pos_old turn_value is_turn_dir_left =
    let raw = mux2 is_turn_dir_left (pos_old -: turn_value) (pos_old +: turn_value) in

    let is_raw_neg = msb raw in
    let is_raw_ge100 = raw >=:. 100 in

    let final = mux2 is_raw_neg (raw +:. 100) (mux2 is_raw_ge100 (raw -:. 100) raw) in

    let hundred = of_int ~width:(width pos_old) 100 in
    let d_to_zero = mux2 is_turn_dir_left pos_old (hundred -: pos_old) in
    let hit_zero = mux2 (pos_old ==:. 0) (zero 1) (mux2 (turn_value >=: d_to_zero) (one 1) (zero 1)) in
    hit_zero, final
  in

  let hit_zero_final, next_value = turn_knob pos_reg turn_eff is_dir_left in

  let next_pos_reg = (mux2 msb_arrived next_value pos_reg) -- "next_pos_reg" in
  assign next_pos_wire next_pos_reg; 

  let is_next_pos_zero = (next_pos_wire ==:. 0) -- "is_next_pos_zero" in

  let stopped_at_zero_ctr = reg_fb spec
    ~enable:vdd
    ~width:32
    ~f:(fun prev -> 
      mux2 clear (zero 32) (mux2 msb_arrived (prev +: uresize is_next_pos_zero 32) prev))
    -- "stopped_at_zero_ctr"
  in

  let hit_zero_ctr = reg_fb spec
    ~enable:vdd
    ~width:32
    ~f:(fun prev ->
      let init_value = zero 32 in
      mux2 clear init_value (mux2 msb_arrived (prev +: (uresize wrap_eff 32 +: uresize hit_zero_final 32)) prev)
      )
    -- "hit_zero_ctr"
  in

  stopped_at_zero_ctr, hit_zero_ctr
;;
