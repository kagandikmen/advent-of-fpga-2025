(*
 *
 * AoF - Testbench for the Solution of Day 4
 * Created:     2025-12-22
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

let parse_line =
  fun (s: string) -> 
    let s = String.strip s in
    if String.is_empty s then None
    else Some s
;;

let day04_test ~max_passes =
  let cycles_per_bit = 4 in

  let lines = In_channel.read_lines "input.txt"
    |> List.filter_map ~f:parse_line
  in
  
  let clock = input "clock" 1 in
  let clear = input "clear" 1 in
  let uart_rx_value = input "uart_rx_value" 1 in

  let total_rolls_collected, is_done = Day04.create_day04_logic
    ~clock
    ~clear
    ~cycles_per_bit
    ~max_passes
    uart_rx_value
  in

  let circuit = Circuit.create_exn
    ~name:"day04"
    [
      output "total_rolls_collected" total_rolls_collected;
      output "is_done" is_done;
    ]
  in

  let _waves, sim = Waveform.create (Cyclesim.create ~config:Cyclesim.Config.trace_all circuit) in

  let vcd_path = "/tmp/day04.vcd" in
  let vcd_oc = Out_channel.create vcd_path in
  let sim = Hardcaml.Vcd.Gtkwave.wrap vcd_oc sim in

  let uart_in = Cyclesim.in_port sim "uart_rx_value" in
  let clear_in = Cyclesim.in_port sim "clear" in
  let total_rolls_collected = Cyclesim.out_port ~clock_edge:Before sim "total_rolls_collected" in
  let is_done = Cyclesim.out_port ~clock_edge:Before sim "is_done" in

  let sim_driver = Simulation.create
    ~sim
    ~uart_in
    ~uart_cycles_per_bit:cycles_per_bit
    ~done_sig:is_done
    ()
  in

  (* stimuli *)

  uart_in := Bits.vdd;
  clear_in := Bits.vdd;
  sim_driver.wait 5; 
  clear_in := Bits.gnd;

  List.iter lines ~f:(fun line ->
    String.iter line ~f:sim_driver.uart.send_ascii_char;
  );

  sim_driver.wait_until_done ~step:1000 ~max_steps:20_000_000;

  let final_trc = Bits.to_int !total_rolls_collected in

  Out_channel.flush vcd_oc;
  Out_channel.close vcd_oc;

  final_trc
;;

let%expect_test "day04_test_step1" =
  let result = day04_test ~max_passes:(one 8) in
  printf "%d\n" result;

  [%expect {|
    1491
  |}]

let%expect_test "day04_test_step2" =
  let result = day04_test ~max_passes:(zero 8) in
  printf "%d\n" result;

  [%expect {|
    8722
  |}]
