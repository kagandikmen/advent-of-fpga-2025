(*
 *
 * AoF - Testbench for the Solution of Day 3 
 * Created:     2025-12-21
 * Modified:    2025-12-21
 * Author:      Kagan Dikmen
 *
 *)

open! Core
open! Hardcaml
open! Hardcaml_arty
open! Hardcaml_waveterm
open! Signal

module Waveform = Hardcaml_waveterm.Waveform

let parse_line = 
  fun (s: string) -> 
    let s = String.strip s in
    if String.is_empty s then None
    else Some s
;;

let%expect_test "day03_test" =
  let cycles_per_bit = 4 in
  let k = 2 in

  let banks = In_channel.read_lines "input.txt"
    |> List.filter_map ~f:parse_line
  in
  
  let clock = input "clock" 1 in
  let clear = input "clear" 1 in
  let uart_rx_value = input "uart_rx_value" 1 in

  (* toj: total output joltage *)
  let toj = Day03.create_day03_logic
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
    ]
  in

  let _waves, sim = Waveform.create (Cyclesim.create ~config:Cyclesim.Config.trace_all circuit) in

  let vcd_path = "/tmp/day03.vcd" in
  let vcd_oc = Out_channel.create vcd_path in
  let sim = Hardcaml.Vcd.Gtkwave.wrap vcd_oc sim in

  let uart_in = Cyclesim.in_port sim "uart_rx_value" in
  let clear_in = Cyclesim.in_port sim "clear" in
  let toj = Cyclesim.out_port ~clock_edge:Before sim "toj" in

  let cycle () = Cyclesim.cycle sim in
  let wait c = for _ = 1 to c do cycle () done in

  let send_bit b = 
    uart_in := (if b = 1 then Bits.vdd else Bits.gnd);
    wait cycles_per_bit;
  in

  let send_byte (byte : int) =
    send_bit 0;
    for i = 0 to 7 do
      send_bit ((byte lsr i) land 1)
    done;
    send_bit 1;
  in

  let send_ascii_char (c: char) =
    send_byte (Char.to_int c);
    wait cycles_per_bit;
  in

  (* stimuli *)

  uart_in := Bits.vdd;
  clear_in := Bits.vdd;
  wait 5;
  clear_in := Bits.gnd;

  List.iter banks ~f:(fun bank ->
    String.iter bank ~f:send_ascii_char;
  );

  let final_toj = Bits.to_int !toj in

  printf "%d" final_toj;

  Out_channel.flush vcd_oc;
  Out_channel.close vcd_oc;

  [%expect {|
    17324
  |}]

;;
