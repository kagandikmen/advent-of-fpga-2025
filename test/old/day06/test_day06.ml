(*
 *
 * AoF - Testbench for the Solution of Day 6
 * Created:     2025-12-26
 * Modified:    2025-12-28
 * Author:      Kagan Dikmen
 *
 *)

open! Core
open! Hardcaml
open! Hardcaml_aof_test
open! Hardcaml_waveterm
open! Signal

module Waveform = Hardcaml_waveterm.Waveform

let%expect_test "day06_test" =
  let cycles_per_bit = 4 in
  
  (* trying out sth new here: *)
  let stream = In_channel.read_all "input.txt"
    |> String.to_list
    |> List.map ~f:Char.to_int
  in

  let clock = input "clock" 1 in
  let clear = input "clear" 1 in
  let uart_rx_value = input "uart_rx_value" 1 in

  let total_p1, total_p2, is_done = Day06_old.Day06.create_day06_logic
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in

  let circuit = Circuit.create_exn
    ~name:"day06"
    [
      output "total_p1" total_p1;
      output "total_p2" total_p2;
      output "is_done" is_done;
    ]
  in

  let _waves, sim = Waveform.create (Cyclesim.create ~config:Cyclesim.Config.trace_all circuit) in

  let vcd_path = "/tmp/day06.vcd" in
  let vcd_oc = Out_channel.create vcd_path in
  let sim = Hardcaml.Vcd.Gtkwave.wrap vcd_oc sim in

  let uart_in = Cyclesim.in_port sim "uart_rx_value" in
  let clear_in = Cyclesim.in_port sim "clear" in
  let total_p1_out = Cyclesim.out_port ~clock_edge:Before sim "total_p1" in
  let total_p2_out = Cyclesim.out_port ~clock_edge:Before sim "total_p2" in
  let is_done_out = Cyclesim.out_port ~clock_edge:Before sim "is_done" in

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

  sim_driver.uart.send_byte 0x02; (* start of text in ascii-ese, but it actually doesn't matter what is sent as long as it is not printable *)

  List.iter stream ~f:sim_driver.uart.send_byte;

  sim_driver.uart.send_byte 0x03; (* end of text *)

  sim_driver.wait_until_done ~step:100 ~max_steps:200_000;

  let final_total_p1 = Bits.to_int !total_p1_out in
  let final_total_p2 = Bits.to_int !total_p2_out in

  printf "%d\n%d\n" final_total_p1 final_total_p2;

  Out_channel.flush vcd_oc;
  Out_channel.close vcd_oc;

  [%expect {|
    5274623692
    50200934826
  |}]
;;
