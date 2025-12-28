(*
 *
 * AoF - Maths Library
 * Created:     2025-12-28
 * Modified:    2025-12-28
 * Author:      Kagan Dikmen
 *
 *)

open! Core
open! Hardcaml
open! Signal

let ascii_to_int8 c = uresize (c -:. Char.to_int '0') 8

let ceil_log2 n = Float.iround_towards_zero_exn(Float.round_up(log(float n) /. log(2.)))

let is_digit c = (c >=:. Char.to_int '0') &: (c <=:. Char.to_int '9')

let max2 a b = mux2 (a >=: b) a b
let min2 a b = mux2 (a <=: b) a b

let mul10 (x: Signal.t) : Signal.t = (Signal.sll x 3) +: (Signal.sll x 1)