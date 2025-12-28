(*
 *
 * AoF - Testbench for the Solution of Day 1
 * Created:     2025-12-12
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

let parse_line =
  fun (s : string) -> 
    let s = String.strip s in
    if String.is_empty s then None
    else (
      let dir = s.[0] in
      let mag = String.subo s ~pos:1 in
      let n = Int.of_string (mag |> String.strip) in
      let v =
        match dir with
        | 'R' -> n
        | 'L' -> -n
        |  _  -> failwithf "Error 10001" ()
      in
      Some v
    )

let%expect_test "day01_test" =
  let cycles_per_bit = 4 in

  let clock = input "clock" 1 in 
  let clear = input "clear" 1 in
  let uart_rx_value = input "uart_rx_value" 1 in

  let stopped_at_zero_ctr, hit_zero_ctr = Day01.create_counting_logic
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in

  let circuit = Circuit.create_exn
    ~name:"day01"
    [
      output "stopped_at_zero_ctr" stopped_at_zero_ctr;
      output "hit_zero_ctr" hit_zero_ctr;
    ]
  in

  let _waves, sim = Waveform.create (Cyclesim.create ~config:Cyclesim.Config.trace_all circuit) in

  let vcd_path = "/tmp/day01.vcd" in 
  let vcd_oc = Out_channel.create vcd_path in
  let sim = Hardcaml.Vcd.Gtkwave.wrap vcd_oc sim in

  let uart_in = Cyclesim.in_port sim "uart_rx_value" in
  let clear_in = Cyclesim.in_port sim "clear" in
  let stopped_at_zero_ctr = Cyclesim.out_port ~clock_edge:Before sim "stopped_at_zero_ctr" in
  let hit_zero_ctr = Cyclesim.out_port ~clock_edge:Before sim "hit_zero_ctr" in

  let sim_driver = Simulation.create
    ~sim
    ~uart_in
    ~uart_cycles_per_bit:cycles_per_bit
    ()
  in

  let send_hw (n : int) = 
    let n_lsb = n land 0xFF in
    let n_msb = ((n land 0xFFFF) lsr 8) land 0xFF in

    sim_driver.uart.send_byte n_lsb;
    uart_in := Bits.vdd;
    
    sim_driver.wait cycles_per_bit;

    sim_driver.uart.send_byte n_msb;
    uart_in := Bits.vdd;
  in

  uart_in := Bits.vdd;
  clear_in := Bits.vdd;
  sim_driver.wait 5;
  clear_in := Bits.gnd;

  let moves = In_channel.read_lines "input.txt"
    |> List.filter_map ~f:parse_line
  in

  List.iter moves ~f:(fun v ->
    send_hw v;
    sim_driver.wait (40 * cycles_per_bit);
  );

  let final_stopped = Bits.to_int !stopped_at_zero_ctr in
  let final_hit = Bits.to_int !hit_zero_ctr in

  printf "Stopped at zero: %d times\n" final_stopped;
  printf "Hit zero: %d times\n" final_hit;

  Out_channel.flush vcd_oc;
  Out_channel.close vcd_oc;

  [%expect {|
    Stopped at zero: 1092 times
    Hit zero: 6616 times
  |}]
;;
