(*
 *
 * AoF - Test Library - Simulation
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
    uart: Uart.t;
    cycle: unit -> unit;
    wait: int -> unit;
    wait_until_done: step:int -> max_steps:int -> unit;
  }

let create ~(sim: ('i, 'o) Cyclesim.t) ~(uart_in: Bits.t ref) ~(uart_cycles_per_bit: int) ?(done_sig: Bits.t ref option) () =

  let uart = Uart.create ~sim ~uart_in ~cycles_per_bit:uart_cycles_per_bit in

  let cycle () = Cyclesim.cycle sim in

  let wait c = for _ = 1 to c do cycle () done in

  let rec wait_until_done ~(step: int) ~(max_steps: int) =
    match done_sig with 
      | None -> failwith "No done signal"
      | Some done_sig_ref -> 
        if max_steps = 0 then failwith "Timeout"
        else (
          wait step;
          if Bits.to_bool !done_sig_ref then ()
          else wait_until_done ~step ~max_steps:(max_steps-1)
        )
  in

  { uart; cycle; wait; wait_until_done; }