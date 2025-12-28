(*
 *
 * AoF - Testbench for the Solution of Day 2
 * Created:     2025-12-15
 * Modified:    2025-12-28
 * Author:      Kagan Dikmen
 *
 *)

open! Core
open! Hardcaml
open! Hardcaml_aof_test
open! Hardcaml_waveterm
open! Signal
open! Stdio

module Waveform = Hardcaml_waveterm.Waveform   

let parse_bounds (s : string) : string list list =
  let s = String.strip s in
  let slist = String.split_on_chars ~on:[ ','; ] s in
  let bounds_list = List.map ~f:(String.split_on_chars ~on:[ '-'; ]) slist in
  bounds_list
;;

let%expect_test "day02_test" =
  let input_text = In_channel.read_all "input.txt" in
  let bounds = parse_bounds input_text in

  (* List.iter bounds ~f:(fun field ->
    List.iter field ~f:(fun v -> printf "%s\n" v)); *)

  let cycles_per_bit = 4 in
  
  let clock = input "clock" 1 in
  let clear = input "clear" 1 in 
  let uart_rx_value = input "uart_rx_value" 1 in

  let sum_step1, sum_step2 = Day02.create_addition_logic
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in

  let circuit = Circuit.create_exn
    ~name:"day02"
    [
      output "sum_step1" sum_step1;
      output "sum_step2" sum_step2;
    ]
  in

  let _waves, sim = Waveform.create (Cyclesim.create ~config:Cyclesim.Config.trace_all circuit) in

  let vcd_path = "/tmp/day02.vcd" in
  let vcd_oc = Out_channel.create vcd_path in
  let sim = Hardcaml.Vcd.Gtkwave.wrap vcd_oc sim in

  let uart_in = Cyclesim.in_port sim "uart_rx_value" in
  let clear_in = Cyclesim.in_port sim "clear" in
  let sum_step1 = Cyclesim.out_port ~clock_edge:Before sim "sum_step1" in
  let sum_step2 = Cyclesim.out_port ~clock_edge:Before sim "sum_step2" in

  let sim_driver = Simulation.create
    ~sim
    ~uart_in
    ~uart_cycles_per_bit:cycles_per_bit
    ()
  in

  let send_int40 (n : int) =
    let n_b0 = n land 0xFF in
    let n_b1 = (n lsr 8) land 0xFF in
    let n_b2 = (n lsr 16) land 0xFF in
    let n_b3 = (n lsr 24) land 0xFF in
    let n_b4 = (n lsr 32) land 0xFF in

    sim_driver.uart.send_byte n_b0;
    uart_in := Bits.vdd;
    sim_driver.wait cycles_per_bit;

    sim_driver.uart.send_byte n_b1;
    uart_in := Bits.vdd;
    sim_driver.wait cycles_per_bit;

    sim_driver.uart.send_byte n_b2;
    uart_in := Bits.vdd;
    sim_driver.wait cycles_per_bit;

    sim_driver.uart.send_byte n_b3;
    uart_in := Bits.vdd;
    sim_driver.wait cycles_per_bit;

    sim_driver.uart.send_byte n_b4;
    uart_in := Bits.vdd;
  in

  (* stimuli *)

  uart_in := Bits.vdd;
  clear_in := Bits.vdd;
  sim_driver.wait 5;
  clear_in := Bits.gnd;

  List.iter bounds ~f:(fun field ->
    let lo = List.nth_exn field 0 |> Int.of_string in
    let hi = List.nth_exn field 1 |> Int.of_string in
    for nr = lo to hi do
      send_int40 nr;
      sim_driver.wait (40 * cycles_per_bit);
    done;
  );

  let final_sum_step1 = Bits.to_int !sum_step1 in
  let final_sum_step2 = Bits.to_int !sum_step2 in

  printf "Step 1: %d\n" final_sum_step1;
  printf "Step 2: %d\n" final_sum_step2;

  Out_channel.flush vcd_oc;
  Out_channel.close vcd_oc;

  [%expect {|
    Step 1: 1010
    Step 2: 4340
  |}]
;;
