(*
 *
 * AoF - Procedures Library
 * Created:     2025-12-28
 * Modified:    2025-12-28
 * Author:      Kagan Dikmen
 *
 *)

open! Core
open! Hardcaml
open! Signal
open! Always

let read_array (a: Always.Variable.t array) ~(idx: Signal.t) =
  let pick i x = mux2 (idx ==:. i) x (zero (width a.(0).value)) in
  List.init (Array.length a) ~f:(fun i ->
    pick i a.(i).value)
  |> List.reduce_exn ~f:(|:)

let write_array (a: Always.Variable.t array) ~(idx: Signal.t) ~(data: Signal.t) =
  List.init (Array.length a) ~f:(fun i ->
    when_ (idx ==:. i)
      [
        a.(i) <-- data;
      ])


