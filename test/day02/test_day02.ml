(*
 *
 * AoF - Testbench for the Solution of Day 2
 * Created:     2026-01-07
 * Modified:    2026-01-12
 * Author:      Kagan Dikmen
 *
 *)

open! Core
open! Hardcaml
open! Hardcaml_aof_test
open! Hardcaml_waveterm
open! Signal

module Waveform = Hardcaml_waveterm.Waveform

let%expect_test "day02_test" =
  let cycles_per_bit = 4 in

  let stream = In_channel.read_all "input.txt"
    |> String.to_list
    |> List.map ~f:Char.to_int
  in

  let clock = input "clock" 1 in
  let clear = input "clear" 1 in
  let uart_rx_value = input "uart_rx_value" 1 in

  let part_1, part_2, is_done, debug_output = Day02.create
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in

  let circuit = Circuit.create_exn
    ~name:"day02"
    [
      output "part_1" part_1;
      output "part_2" part_2;
      output "is_done" is_done;
      output "debug_output" debug_output;
    ]
  in

  let _waves, sim = Waveform.create (Cyclesim.create ~config:Cyclesim.Config.trace_all circuit) in

  let vcd_path = "/tmp/day02.vcd" in
  let vcd_oc = Out_channel.create vcd_path in
  let sim = Hardcaml.Vcd.Gtkwave.wrap vcd_oc sim in

  let uart_in = Cyclesim.in_port sim "uart_rx_value" in
  let clear_in = Cyclesim.in_port sim "clear" in
  let part_1_out = Cyclesim.out_port ~clock_edge:Before sim "part_1" in
  let part_2_out = Cyclesim.out_port ~clock_edge:Before sim "part_2" in
  let is_done_out = Cyclesim.out_port ~clock_edge:Before sim "is_done" in
  let _debug_out = Cyclesim.out_port ~clock_edge:Before sim "debug_output" in

  let sim_driver = Simulation.create
    ~sim
    ~uart_in
    ~uart_cycles_per_bit:cycles_per_bit
    ~done_sig:is_done_out
    ()
  in

  uart_in := Bits.vdd;
  clear_in := Bits.vdd;
  sim_driver.wait 5;
  clear_in := Bits.gnd;

  sim_driver.uart.send_byte 0x02; (* ascii for start of text *)

  List.iter stream ~f:sim_driver.uart.send_byte;

  sim_driver.uart.send_byte 0x03; (* ascii for end of text *)

  sim_driver.wait_until_done ~step:50 ~max_steps:200_000;

  let final_part_1 = Bits.to_int !part_1_out in
  let final_part_2 = Bits.to_int !part_2_out in

  printf "%d\n%d\n" final_part_1 final_part_2;

  Out_channel.flush vcd_oc;
  Out_channel.close vcd_oc;

  [%expect {|
    28846518423
    31578210022
  |}]
;;
