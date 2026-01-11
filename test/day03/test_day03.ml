(*
 *
 * AoF - Testbench for the Solution of Day 3 
 * Created:     2025-12-21
 * Modified:    2026-01-11
 * Author:      Kagan Dikmen
 *
 *)

open! Core
open! Hardcaml
open! Hardcaml_aof_test
open! Hardcaml_waveterm
open! Signal

module Waveform = Hardcaml_waveterm.Waveform

let day03_test ~k =
  let cycles_per_bit = 4 in

  let stream = In_channel.read_all "input.txt"
    |> String.to_list
    |> List.map ~f:Char.to_int
  in
  
  let clock = input "clock" 1 in
  let clear = input "clear" 1 in
  let uart_rx_value = input "uart_rx_value" 1 in

  (* toj: total output joltage *)
  let toj, is_done = Day03.create_day03_logic
    ~clock
    ~clear
    ~cycles_per_bit
    ~k
    uart_rx_value
  in

  let circuit = Circuit.create_exn
    ~name:"day03"
    [
      output "toj" toj;
      output "is_done" is_done;
    ]
  in

  let _waves, sim = Waveform.create (Cyclesim.create ~config:Cyclesim.Config.trace_all circuit) in

  let vcd_path = "/tmp/day03.vcd" in
  let vcd_oc = Out_channel.create vcd_path in
  let sim = Hardcaml.Vcd.Gtkwave.wrap vcd_oc sim in

  let uart_in = Cyclesim.in_port sim "uart_rx_value" in
  let clear_in = Cyclesim.in_port sim "clear" in
  let toj = Cyclesim.out_port ~clock_edge:Before sim "toj" in
  let is_done_out = Cyclesim.out_port ~clock_edge:Before sim "is_done" in

  let sim_driver = Simulation.create
    ~sim
    ~uart_in
    ~uart_cycles_per_bit:cycles_per_bit
    ~done_sig:is_done_out
    ()
  in

  (* stimuli *)

  uart_in := Bits.vdd;
  clear_in := Bits.vdd;
  sim_driver.wait 5;
  clear_in := Bits.gnd;

  sim_driver.uart.send_byte 0x02; (* ascii for start of text *)

  List.iter stream ~f:sim_driver.uart.send_byte;

  sim_driver.uart.send_byte 0x03; (* ascii for end of text *)

  sim_driver.wait_until_done ~step:50 ~max_steps:200_000;

  let final_toj = Bits.to_int !toj in

  Out_channel.flush vcd_oc;
  Out_channel.close vcd_oc;

  final_toj
;;

let%expect_test "day03_test_k2" =
  let result = day03_test ~k:2 in
  printf "%d\n" result;

  [%expect {|
    17324
  |}]

let%expect_test "day03_test_k12" =
  let result = day03_test ~k:12 in
  printf "%d\n" result;

  [%expect {|
    171846613143331
  |}]
