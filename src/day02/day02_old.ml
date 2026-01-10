(*
 *
 * AoF - Hardcaml Solution for Day 2 (Step 1 & Step 2)
 * Created:     2025-12-15
 * Modified:    2026-01-10
 * Author:      Kagan Dikmen
 *
 *)

(*************************** IMPORTANT ****************************)
(* This is the older & much-slower implementation.                *)
(* For the newer solution, see day02_newer.ml                     *)
(******************************************************************)

(* TODO: Add FPGA build and RTL generation logic *)

open! Core
open! Hardcaml
open! Hardcaml_aof
open! Hardcaml_arty
open! Signal

let u64 x = uresize x 64


let list_slice lst i j = 
  List.take (List.drop lst i) (j-i+1)
;;


let list_comp (x: Signal.t list) (y: Signal.t list) : Signal.t =
  List.map2_exn x y ~f:(fun a b -> a ==: b)
  |> List.fold ~init:vdd ~f:(&:)
;;


let list_all_eq (lst : Signal.t list) : Signal.t =
  match lst with
  | [] | [_] -> vdd
  | x0 :: rest ->
    List.map rest ~f:(fun x -> x ==: x0)
    |> List.reduce_exn ~f:(&:)
;;


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


(* step 1 *)
let is_silly_number (x40: Signal.t) : Signal.t =
  let bcd10 = convert_bin40_to_bcd10 x40 in

  let num_leading_zeros = List.fold bcd10 ~init:((zero 4), vdd) ~f:(fun (acc, sf) d ->
    let sf' = sf &: (d ==:. 0) in
    let acc' = mux2 sf' (acc +:. 1) acc in
    (acc', sf'))
    |> fst
  in

  mux2 (num_leading_zeros ==:. 0) (list_comp (list_slice bcd10 0 4) (list_slice bcd10 5 9))
    (mux2 (num_leading_zeros ==:. 2) (list_comp (list_slice bcd10 2 5) (list_slice bcd10 6 9))
      (mux2 (num_leading_zeros ==:. 4) (list_comp (list_slice bcd10 4 6) (list_slice bcd10 7 9))
        (mux2 (num_leading_zeros ==:. 6) (list_comp (list_slice bcd10 6 7) (list_slice bcd10 8 9))
          (mux2 (num_leading_zeros ==:. 8) (list_comp (list_slice bcd10 8 8) (list_slice bcd10 9 9))
            gnd))))
;;


(* step 2 *)
let is_goofy_number (x40: Signal.t) : Signal.t =
  let bcd10 = convert_bin40_to_bcd10 x40 in

  let b0 = List.nth_exn bcd10 9 in    let b1 = List.nth_exn bcd10 8 in
  let b2 = List.nth_exn bcd10 7 in    let b3 = List.nth_exn bcd10 6 in
  let b4 = List.nth_exn bcd10 5 in    let b5 = List.nth_exn bcd10 4 in
  let b6 = List.nth_exn bcd10 3 in    let b7 = List.nth_exn bcd10 2 in
  let b8 = List.nth_exn bcd10 1 in    let b9 = List.nth_exn bcd10 0 in

  let num_leading_zeros = List.fold bcd10 ~init:((zero 4), vdd) ~f:(fun (acc, sf) d ->
    let sf' = sf &: (d ==:. 0) in
    let acc' = mux2 sf' (acc +:. 1) acc in
    (acc', sf'))
    |> fst
  in

  let num_real_digits = (of_int ~width:(width num_leading_zeros) 10) -: num_leading_zeros in

  let all_real_digits_equal = 
    List.init 11 ~f:(fun k -> 
      let slice = list_slice bcd10 k 9 in
      (num_leading_zeros ==:. k) &: (list_all_eq slice))
    |> List.reduce_exn ~f:(|:)
  in
 
  let cond_10' = (list_all_eq [ b9; b7; b5; b3; b1; ] &: list_all_eq [ b8; b6; b4; b2; b0; ])
               |: (list_all_eq [ b9; b4; ] &: list_all_eq [ b8; b3; ] &: list_all_eq [ b7; b2; ] &: list_all_eq [ b6; b1; ] &: list_all_eq [ b5; b0; ]) in
  
  let cond_09' = list_all_eq [ b8; b5; b2; ] &: list_all_eq [ b7; b4; b1; ] &: list_all_eq [ b6; b3; b0; ] in

  let cond_08' = (list_all_eq [ b7; b5; b3; b1; ] &: list_all_eq [ b6; b4; b2; b0;])
               |: (list_all_eq [ b7; b3; ] &: list_all_eq [ b6; b2; ] &: list_all_eq [ b5; b1; ] &: list_all_eq [ b4; b0; ]) in

  let cond_06' = (list_all_eq [ b5; b3; b1; ] &: list_all_eq [ b4; b2; b0; ])
                |: (list_all_eq [ b5; b2; ] &: list_all_eq [ b4; b1; ] &: list_all_eq [ b3; b0; ]) in

  let cond_04' = list_all_eq [ b3; b1; ] &: list_all_eq [ b2; b0; ] in

  let cond_10 = cond_10' &: (num_real_digits ==:. 10) in
  let cond_09 = cond_09' &: (num_real_digits ==:. 9) in
  let cond_08 = cond_08' &: (num_real_digits ==:. 8) in
  let cond_06 = cond_06' &: (num_real_digits ==:. 6) in
  let cond_04 = cond_04' &: (num_real_digits ==:. 4) in

  all_real_digits_equal |: cond_10 |: cond_09 |: cond_08 |: cond_06 |: cond_04
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
  let is_value_goofy = is_goofy_number value_40 -- "is_value_goofy" in

  let sum_step1 = reg_fb spec
    ~enable:vdd
    ~width:64
    ~f:(fun prev ->
        mux2 b4_arrived (mux2 is_value_silly (prev +: u64 value_40) prev) prev)
    -- "sum_step1"
  in

  let sum_step2 = reg_fb spec
    ~enable:vdd
    ~width:64
    ~f:(fun prev ->
        mux2 b4_arrived (mux2 is_value_goofy (prev +: u64 value_40) prev) prev)
    -- "sum_step2"
  in

  sum_step1, sum_step2
;;