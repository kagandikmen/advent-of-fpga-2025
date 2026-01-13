(*
 *
 * AoF - Arty Top Module & RTL Generator for Day 5
 * Created:     2026-01-13
 * Modified:    2026-01-13
 * Author:      Kagan Dikmen
 *
 *)
 
open! Base
open! Hardcaml
open! Hardcaml_arty
open! Signal

let create _scope (input : _ User_application.I.t) =
  let _p1, _p2, is_done =
    Day05.create_logic
      ~clock:input.clk_166
      ~clear:~:(input.clear_n_166)
      input.uart_rx
  in

  let led_4bits = zero 4 in
  
  let led_rgb =
    List.init 4 ~f:(fun _ ->
      User_application.Led_rgb.
        { 
          r = zero 1;
          g = is_done;
          b = zero 1;
        })
  in

  let uart_tx = { With_valid.valid = gnd; value = zero 8 } in

  User_application.O.
    {
      led_4bits;
      uart_tx;
      led_rgb;
      ethernet = User_application.Ethernet.O.unused (module Signal);
    }
;;

let () =
  Hardcaml_arty.Rtl_generator.generate
    ~instantiate_ethernet_mac:false
    create
    (To_channel Stdio.stdout)
;;
