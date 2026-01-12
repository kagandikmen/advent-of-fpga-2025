(*
 *
 * AoF - Hardcaml Solution for Day 3
 * Created:     2025-12-19
 * Modified:    2026-01-13
 * Author:      Kagan Dikmen
 *
 *)

open! Core
open! Hardcaml
open! Hardcaml_aof
open! Hardcaml_arty
open! Signal

let u64 x = uresize x 64
let s64 x = sresize x 64

let least_significant_digit (digits: Signal.t list) (len: Signal.t) : Signal.t = 
  let k = List.length digits in

  let rec go j =
    if j = 0 then zero 4
    else
      let dj = List.nth_exn digits (j-1) in
      mux2 (len ==:. j) dj ( go(j-1) )
  in

  go k
;;

let insert_digit ~(digits: Signal.t list) ~(len: Signal.t) ~(insert_en: Signal.t) ~(d: Signal.t) : Signal.t list = 
  List.mapi digits ~f:(fun i di ->
    mux2 (insert_en &: (len ==:. i)) d di)
;;

let pop_digits ~(digits: Signal.t list) ~(k: int) ~(len0: Signal.t) ~(drops_left0: Signal.t) ~(d: Signal.t) : Signal.t * Signal.t =
  let step (len, drops) =
    let least_significant_digit = least_significant_digit digits len in
    let can_pop = (len >:. 0) &: (drops >:. 0) &: (least_significant_digit <: d) in
    let len' = mux2 can_pop (len -:. 1) len in
    let drops' = mux2 can_pop (drops -:. 1) drops in
    (len', drops')
  in
  List.fold (List.init k ~f:Fn.id) ~init:(len0, drops_left0) ~f:(fun acc _ -> step acc)  (* run step k times over initial value*)
;;

let digits_to_number (digits: Signal.t list) : Signal.t =
  List.fold digits ~init:(zero 64) ~f:(fun acc d -> Math.mul10 acc +: u64 d)
;;

let create_logic ~clock ~clear ~k (uart_rx: Signal.t Uart.Byte_with_valid.t) =
  let n = 100 in
  let drops_max = n - k in

  let _uart_value = uart_rx.value -- "uart_value" in
  let _uart_valid = uart_rx.valid -- "uart_valid" in

  let spec = Reg_spec.create
    ~clock
    ~clear
    ()
  in

  let step_en = (uart_rx.valid &: (uart_rx.value >=:. Char.to_int '0') &: (uart_rx.value <=:. Char.to_int '9')) -- "step_en" in

  let digit = uresize (uart_rx.value -:. Char.to_int '0') 4 in

  let _idx = reg_fb spec
    ~enable:vdd
    ~width:7
    ~f:(fun prev ->
      mux2 clear (zero 7) (mux2 step_en (mux2 (prev ==:. (n-1)) (zero 7) (prev +:. 1)) prev))
    -- "idx"
  in

  let is_last_digit_of_bank = (uart_rx.valid &: ((uart_rx.value ==:. Char.to_int '\n') |: (uart_rx.value ==:. 0x03))) -- "is_last_digit_of_bank" in

  let is_etx_received = reg_fb spec
    ~enable:vdd
    ~width:1
    ~f:(fun prev ->
      mux2 uart_rx.valid (prev |: (uart_rx.value ==:. 0x03)) prev)
    -- "is_etx_received"
  in

  let len_w = wire 5 in
  let len_reg = (reg spec len_w) -- "len_reg" in

  let drops_left_w = wire 8 in
  let drops_left_reg = reg (Reg_spec.override spec ~clear_to:(Signal.of_int ~width:8 drops_max)) drops_left_w -- "drops_left_reg" in

  let digits_w = List.init k ~f:(fun i -> (wire 4) -- ("digit_w_" ^ Int.to_string i)) in
  let digits_reg = List.mapi digits_w ~f:(fun i w -> (reg spec w) -- ("digit_" ^ Int.to_string i)) in

  let len_next = ref len_reg in
  let drops_left_reg_next = ref drops_left_reg in
  let digits_next = ref digits_reg in

  let _len_n_int, _dl_n_int, digits_n_int =
    let len_p, drops_left_p = pop_digits
      ~digits:digits_reg
      ~k
      ~len0:len_reg
      ~drops_left0:drops_left_reg
      ~d:digit
    in

    let is_stack_full = (len_p ==:. k) in
    let insert_en = step_en &: (~:is_stack_full) in
    let drop_en = step_en &: is_stack_full &: (drops_left_p >:. 0) in

    let digits_after_insert = insert_digit
      ~digits:digits_reg
      ~len:len_p
      ~insert_en
      ~d:digit
    in

    let len_after_insert = mux2 insert_en (len_p +:. 1) len_p in
    let drops_left_after_insert = mux2 drop_en (drops_left_p -:. 1) drops_left_p in

    let len_next_int = mux2 step_en len_after_insert !len_next in
    let drops_left_reg_next_int = mux2 step_en drops_left_after_insert !drops_left_reg_next in
    let digits_next_int = List.map2_exn !digits_next digits_after_insert ~f:(fun ol nu -> mux2 step_en nu ol) in

    len_next := mux2 (clear |: is_last_digit_of_bank) (zero 5) len_next_int;
    drops_left_reg_next := mux2 (clear |: is_last_digit_of_bank) (Signal.of_int ~width:8 drops_max) drops_left_reg_next_int;
    digits_next := List.map digits_next_int ~f:(fun di -> mux2 (clear |: is_last_digit_of_bank) (zero 4) di);

    (len_next_int, drops_left_reg_next_int, digits_next_int)
  in

  assign len_w !len_next;
  assign drops_left_w !drops_left_reg_next;
  List.iter2_exn digits_w !digits_next ~f:assign;

  let current_bank_max_joltage = (digits_to_number digits_n_int) -- "current_bank_max_joltage" in

  let total_output_joltage = reg_fb spec
    ~enable:vdd
    ~width:64
    ~f:(fun prev ->
      let next = prev +: u64 current_bank_max_joltage in
      mux2 clear (zero 64) (mux2 is_last_digit_of_bank next prev))
    -- "total_output_joltage"
  in

  total_output_joltage, is_etx_received
;;

let create ~clock ~clear ~cycles_per_bit ~k uart_rx_value =
  let uart_rx = Uart.Expert.create_rx_state_machine
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in
  create_logic ~clock ~clear ~k uart_rx
;;