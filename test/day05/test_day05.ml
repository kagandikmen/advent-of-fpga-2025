(*
 *
 * AoF - Testbench for the Solution of Day 5
 * Created:     2025-12-24
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

module Package = struct
  type t =
  {
    flag: Bits.t;     (* 0x01: lower bound, 0x02: upper bound, 0x03: section change, 0x04: ingredient id, 0xFF: EOF, others: invalid *)
    payload: Bits.t;  (* 64-bit *)
  }

  let create ~flag ~payload =
    if Bits.width flag <> 8 then invalid_arg "Error 10001";
    if Bits.width payload <> 64 then invalid_arg "Error 10002";
    { flag; payload; }
end

let bits8 (x: int) = Bits.of_int ~width:8 x
let bits64_of_int (x: int) = Bits.of_int ~width:64 x
let bits64_of_int64 (x: int64) = Bits.of_int64 ~width:64 x

let parse (s: string) : Package.t list option =
  let s = String.strip s in
  if String.is_empty s then
    Some
      [
        Package.create ~flag:(bits8 0x03) ~payload:(bits64_of_int 0);
      ]
  else (
    match String.split ~on:'-' s with
    | [ lo; hi; ] -> 
      let lo = String.strip lo |> Int64.of_string in
      let hi = String.strip hi |> Int64.of_string in
      Some
        [
          Package.create ~flag:(bits8 0x01) ~payload:(bits64_of_int64 lo);
          Package.create ~flag:(bits8 0x02) ~payload:(bits64_of_int64 hi);
        ]
    | [ id ] ->
      let id = String.strip id |> Int64.of_string in
      Some
        [
          Package.create ~flag:(bits8 0x04) ~payload:(bits64_of_int64 id);
        ]
    | _ -> failwith "Error 10003"
  )

let%expect_test "day05_test" =
  let cycles_per_bit = 4 in

  let pkgs = In_channel.read_lines "input.txt"
    |> List.filter_map ~f:parse
    |> List.concat
  in

  let clock = input "clock" 1 in
  let clear = input "clear" 1 in
  let uart_rx_value = input "uart_rx_value" 1 in

  let num_fresh, num_covered_ids, is_done = Day05.create_day05_logic
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in

  let circuit = Circuit.create_exn
    ~name:"day05"
    [
      output "num_fresh" num_fresh;
      output "num_covered_ids" num_covered_ids;
      output "is_done" is_done;
    ]
  in

  let _waves, sim = Waveform.create (Cyclesim.create ~config:Cyclesim.Config.trace_all circuit) in

  let vcd_path = "/tmp/day05.vcd" in
  let vcd_oc = Out_channel.create vcd_path in
  let sim = Hardcaml.Vcd.Gtkwave.wrap vcd_oc sim in

  let uart_in = Cyclesim.in_port sim "uart_rx_value" in
  let clear_in = Cyclesim.in_port sim "clear" in
  let num_fresh_out = Cyclesim.out_port ~clock_edge:Before sim "num_fresh" in
  let num_covered_ids_out = Cyclesim.out_port ~clock_edge:Before sim "num_covered_ids" in
  let is_done_out = Cyclesim.out_port ~clock_edge:Before sim "is_done" in

  let sim_driver = Simulation.create
    ~sim
    ~uart_in
    ~uart_cycles_per_bit:cycles_per_bit
    ~done_sig:is_done_out
    ()
  in

  let send_u64 (u64: Bits.t) =
    if Bits.width u64 <> 64 then failwith "Error 10004";
    for i = 0 to 7 do
      let lo = i * 8 in
      let hi = lo + 7 in
      let b = Bits.select u64 hi lo |> Bits.to_int in
      sim_driver.uart.send_byte b;
    done;
  in

  let send_pkg (pkg: Package.t) =
    sim_driver.uart.send_byte (Bits.to_int pkg.flag);
    sim_driver.wait cycles_per_bit;
    send_u64 pkg.payload;
    sim_driver.wait cycles_per_bit;
  in

  (* stimuli *)

  uart_in := Bits.vdd;
  clear_in := Bits.vdd;
  sim_driver.wait 5; 
  clear_in := Bits.gnd;

  List.iter pkgs ~f:send_pkg;

  (* send eof signal *)
  send_pkg (Package.create ~flag:(bits8 0xFF) ~payload:(Bits.zero 64));

  sim_driver.wait_until_done ~step:100 ~max_steps:20_000;

  let final_num_fresh = Bits.to_int !num_fresh_out in
  let final_num_covered_ids = Bits.to_int !num_covered_ids_out in

  printf "%d\n%d\n" final_num_fresh final_num_covered_ids;

  Out_channel.flush vcd_oc;
  Out_channel.close vcd_oc;

  [%expect {|
    828
    352681648086146
  |}]
;;
