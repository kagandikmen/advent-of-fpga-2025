(*
 *
 * AoF - Test Library - UART Procedures
 * Created:     2025-12-28
 * Modified:    2025-12-28
 * Author:      Kagan Dikmen
 *
 *)

open! Core
open! Hardcaml
open! Signal

type t =
  {
    cycle: unit -> unit;
    wait: int -> unit;
    send_bit: int -> unit;
    send_byte: int -> unit;
    send_ascii_char: char -> unit;
  }

let create ~(sim: ('i, 'o) Cyclesim.t) ~(uart_in: Bits.t ref) ~(cycles_per_bit: int) : t =

  let cycle () = Cyclesim.cycle sim in

  let wait c = for _ = 1 to c do cycle () done in

  let send_bit b =
    uart_in := (if b = 1 then Bits.vdd else Bits.gnd);
    wait cycles_per_bit;
  in

  let send_byte (byte: int) =
    send_bit 0;
    for i = 0 to 7 do
      send_bit ((byte lsr i) land 1)
    done;
    send_bit 1;
    send_bit 1;
  in

  let send_ascii_char (c: char) =
    send_byte (Char.to_int c);
    wait cycles_per_bit;
  in

  { cycle; wait; send_bit; send_byte; send_ascii_char; }