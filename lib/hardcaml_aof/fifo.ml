(*
 *
 * AoF - FIFO Library
 * Created:     2026-01-12
 * Modified:    2026-01-12
 * Author:      Kagan Dikmen
 *
 *)

open! Core
open! Hardcaml
open! Signal
open! Always

let create ~clock ~clear ~depth ~width ~(wr_en: Signal.t) ~(wr_data: Signal.t) ~(rd_en: Signal.t) =
  let spec = Reg_spec.create ~clock ~clear () in

  let addr_w = Math.ceil_log2 depth in
  let cnt_w = addr_w + 1 in

  let wr_ptr = Variable.reg spec ~width:addr_w in
  let rd_ptr = Variable.reg spec ~width:addr_w in
  let count = Variable.reg spec ~width:cnt_w in

  let empty = (count.value ==:. 0) in
  let full = (count.value ==:. depth) in

  let do_wr = wr_en &: ~:full in
  let do_rd = rd_en &: ~:empty in

  let waddr = Variable.wire ~default:(zero addr_w) in
  let raddr = Variable.wire ~default:(zero addr_w) in
  let we = Variable.wire ~default:gnd in
  let wdata = Variable.wire ~default:(zero width) in

  let write_port : Signal.write_port =
    {
      write_clock = clock;
      write_enable = we.value;
      write_address = waddr.value;
      write_data = wdata.value;
    }
  in

  let read_port : Signal.read_port =
    { 
      read_clock = clock;
      read_enable = vdd;
      read_address = raddr.value;
    }
  in

  let ram =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:depth
      ~write_ports:[| write_port |]
      ~read_ports:[| read_port |]
      ()
  in

  let rd_data = ram.(0) in

  let delta = (mux2 do_wr (one cnt_w) (zero cnt_w)) -: (mux2 do_rd (one cnt_w) (zero cnt_w)) in

  compile
    [
      we <--. 0;
      waddr <-- wr_ptr.value;
      wdata <-- wr_data;
      raddr <-- rd_ptr.value;

      when_ do_wr
        [
          we <--. 1;
          waddr <-- wr_ptr.value;
          wdata <-- wr_data;
          wr_ptr <-- (wr_ptr.value +:. 1);
        ];

      when_ do_rd
        [
          rd_ptr <-- (rd_ptr.value +:. 1);
        ];

      count <-- (count.value +: delta);
    ];
  
  rd_data, empty, full