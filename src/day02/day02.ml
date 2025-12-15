(*
 *
 * AoF - Hardcaml Solution for Day 1 (Step 1 & Step 2)
 * Created:     2025-12-15
 * Modified:    2025-12-15
 * Author:      Kagan Dikmen
 *
 *)

(* TODO: Add FPGA build and RTL generation logic *)

open! Core
open! Hardcaml
open! Hardcaml_arty
open! Signal

let u64 x = uresize x 64

let convert_bin40_to_bcd10 (x : Signal.t) : Signal.t list =
  let digits = 10 in

  let add3_if_ge5 x = mux2 (x >=:. 5) (x +:. 3) x in

  let bcd_init = zero (digits * 4) in

  let step bcd bit = 
    let bcd_adj = List.init digits ~f:(fun i ->
      let slice_idx = digits - i - 1 in
      let hi = slice_idx * 4 + 3 in
      let lo = slice_idx * 4 in
      let d = select bcd hi lo in
      add3_if_ge5 d)
      |> concat_msb
    in

    let bcd_shifted = (select bcd_adj 38 0) @: bit in
    bcd_shifted
  in

  let bcd = List.fold (List.init 40 ~f:(fun i -> bit x (39-i))) ~init:bcd_init ~f:step in

  List.init digits ~f:(fun i -> 
    let slice_idx = digits - i - 1 in
    let hi = slice_idx * 4 + 3 in
    let lo = slice_idx * 4 in
    select bcd hi lo
    )
;;

let is_silly_number (x40: Signal.t) : Signal.t =
  let bcd10 = convert_bin40_to_bcd10 x40 in

  let num_leading_zeros = List.fold bcd10 ~init:((zero 4), vdd) ~f:(fun (acc, sf) d ->
    let sf' = sf &: (d ==:. 0) in
    let acc' = mux2 sf' (acc +:. 1) acc in
    (acc', sf'))
    |> fst
  in

  let list_slice lst i j = List.take (List.drop lst i) (j-i+1) in

  let list_eq (x: Signal.t list) (y: Signal.t list) : Signal.t =
    List.map2_exn x y ~f:(fun a b -> a ==: b)
    |> List.fold ~init:vdd ~f:(&:)
  in

  mux2 (num_leading_zeros ==:. 0) (list_eq (list_slice bcd10 0 4) (list_slice bcd10 5 9))
    (mux2 (num_leading_zeros ==:. 2) (list_eq (list_slice bcd10 2 5) (list_slice bcd10 6 9))
      (mux2 (num_leading_zeros ==:. 4) (list_eq (list_slice bcd10 4 6) (list_slice bcd10 7 9))
        (mux2 (num_leading_zeros ==:. 6) (list_eq (list_slice bcd10 6 7) (list_slice bcd10 8 9))
          (mux2 (num_leading_zeros ==:. 8) (list_eq (list_slice bcd10 8 8) (list_slice bcd10 9 9))
            gnd))))
;;

let create_addition_logic ~clock ~clear ~cycles_per_bit uart_rx_value =

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
    ~clear
    ()
  in

  let byte_ctr = reg_fb spec
    ~enable:vdd 
    ~width:3
    ~f:(fun prev->
      mux2 uart_rx.valid (mux2 (prev ==:. 4) (zero 3) (prev +:. 1)) prev)
    -- "byte_ctr"
  in

  let reg_b0 = reg_fb spec ~enable:vdd ~width:8 ~f:(fun prev -> mux2 (uart_rx.valid &: (byte_ctr ==:. 0)) uart_rx.value prev) -- "reg_b0" in
  let reg_b1 = reg_fb spec ~enable:vdd ~width:8 ~f:(fun prev -> mux2 (uart_rx.valid &: (byte_ctr ==:. 1)) uart_rx.value prev) -- "reg_b1" in
  let reg_b2 = reg_fb spec ~enable:vdd ~width:8 ~f:(fun prev -> mux2 (uart_rx.valid &: (byte_ctr ==:. 2)) uart_rx.value prev) -- "reg_b2" in
  let reg_b3 = reg_fb spec ~enable:vdd ~width:8 ~f:(fun prev -> mux2 (uart_rx.valid &: (byte_ctr ==:. 3)) uart_rx.value prev) -- "reg_b3" in

  let b4_arrived = (uart_rx.valid &: (byte_ctr ==:. 4)) -- "b4_arrived" in

  let value_40 = concat_msb [ uart_rx.value; reg_b3; reg_b2; reg_b1; reg_b0; ] -- "value_40" in
  let is_value_silly = is_silly_number value_40 -- "is_value_silly" in

  let sum_step1 = reg_fb spec
    ~enable:vdd
    ~width:64
    ~f:(fun prev ->
        mux2 b4_arrived (mux2 is_value_silly (prev +: u64 value_40) prev) prev)
    -- "sum_step1"
  in

  let sum_step2 = reg spec
    value_40 (* placeholder *)
    -- "sum_step2"
  in

  sum_step1, sum_step2
;;