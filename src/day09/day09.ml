(*
 *
 * AoF - Hardcaml Solution for Day 9
 * Created:     2026-01-02
 * Modified:    2026-01-15
 * Author:      Kagan Dikmen
 *
 *)

open! Core
open! Hardcaml
open! Hardcaml_aof
open! Hardcaml_arty
open! Signal

module States = struct
  type t =
    | Idle
    | Receive
    | Compute
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

let abs a b = mux2 (a >=: b) (a -: b) (b -: a)

let min a b = mux2 (a <=: b) a b
let max a b = mux2 (a >=: b) a b

let read_array = Procedure.read_array
let write_array = Procedure.write_array

let create_logic ~clock ~clear ~max_redtiles (uart_rx: Signal.t Uart.Byte_with_valid.t) =
  let open Always in

  let redtile_addr_w = Math.ceil_log2 max_redtiles in
  let ram_depth = 1 lsl (Math.ceil_log2 max_redtiles) in

  let spec = Reg_spec.create
    ~clock
    ~clear
    ()
  in

  let sm = State_machine.create (module States) spec in

  let p1_result = Variable.reg spec ~width:64 in
  let p2_result = Variable.reg spec ~width:64 in

  let x_coords = Array.init max_redtiles ~f:(fun _ -> Variable.reg spec ~width:32) in
  let y_coords = Array.init max_redtiles ~f:(fun _ -> Variable.reg spec ~width:32) in

  let num_redtiles = Variable.reg spec ~width:redtile_addr_w in

  let pack_border ~(x: Signal.t) ~(y: Signal.t) =
    (uresize x 32) @: (uresize y 32)
  in

  let unpack_border_x a = select a 63 32 in
  let unpack_border_y a = select a 31 0 in

  let ram_raddr0 = Variable.wire ~default:(zero redtile_addr_w) in
  let ram_raddr1 = Variable.wire ~default:(zero redtile_addr_w) in
  let ram_waddr = Variable.wire ~default:(zero redtile_addr_w) in
  let ram_we = Variable.wire ~default:gnd in
  let ram_wdata = Variable.wire ~default:(zero 64) in

  let write_port : Signal.write_port =
    {
      write_clock = clock;
      write_enable = ram_we.value;
      write_address = ram_waddr.value;
      write_data = ram_wdata.value;
    }
  in

  let read_port0 : Signal.read_port =
    {
      read_clock = clock;
      read_enable = vdd;
      read_address = ram_raddr0.value;
    }
  in

  let read_port1 : Signal.read_port =
    {
      read_clock = clock;
      read_enable = vdd;
      read_address = ram_raddr1.value;
    }
  in

  let ram_rdata = Ram.create
    ~collision_mode:Read_before_write
    ~size:ram_depth
    ~write_ports:[| write_port |]
    ~read_ports:[| read_port0; read_port1; |]
    ()
  in

  let redtile0 = ram_rdata.(0) in
  let redtile1 = ram_rdata.(1) in

  let p_compute = Variable.reg spec ~width:redtile_addr_w in
  let q_compute = Variable.reg spec ~width:redtile_addr_w in
  
  let find_area ~(a: Signal.t) ~(b: Signal.t) =
    let xa = read_array x_coords ~idx:a in
    let ya = read_array y_coords ~idx:a in
    let xb = read_array x_coords ~idx:b in
    let yb = read_array y_coords ~idx:b in
    
    let dx = abs xa xb in
    let dy = abs ya yb in

    (dx +:. 1) *: (dy +:. 1)
  in

  let is_between ~(a:Signal.t) ~(b:Signal.t) ~(q1:Signal.t) ~(q2:Signal.t) =
    let xa = read_array x_coords ~idx:a in
    let ya = read_array y_coords ~idx:a in
    let xb = read_array x_coords ~idx:b in
    let yb = read_array y_coords ~idx:b in

    let xmin = min xa xb in
    let xmax = max xa xb in
    let ymin = min ya yb in
    let ymax = max ya yb in

    let xq1 = unpack_border_x q1 in
    let yq1 = unpack_border_y q1 in
    let xq2 = unpack_border_x q2 in
    let yq2 = unpack_border_y q2 in

    let is_qline_vertical = xq1 ==: xq2 in
    let is_qline_horizontal = yq1 ==: yq2 in

    let qline_ymin = min yq1 yq2 in
    let qline_ymax = max yq1 yq2 in
    let qline_xmin = min xq1 xq2 in
    let qline_xmax = max xq1 xq2 in

    let does_overlap_vertical = (max qline_ymin ymin) <: (min qline_ymax ymax) in
    let does_overlap_horizontal = (max qline_xmin xmin) <: (min qline_xmax xmax) in

    let does_cross_vertical = (xmin <: xq1) &: (xq1 <: xmax) &: does_overlap_vertical in
    let does_cross_horizontal = (ymin <: yq1) &: (yq1 <: ymax) &: does_overlap_horizontal in

    (is_qline_vertical &: does_cross_vertical) |: (is_qline_horizontal &: does_cross_horizontal)
  in


  let p1_done = Variable.reg spec ~width:1 in
  let p2_done = Variable.reg spec ~width:1 in

  let done_ctr = Variable.reg spec ~width:8 in
  let done_th = 100 in

  let cur_value_receive = Variable.reg spec ~width:32 in
  let coord_sel_receive = Variable.reg spec ~width:1 in
  let tmp_x_receive = Variable.reg spec ~width:32 in

  let mod_num_redtiles (a: Signal.t) =
    mux2 (a >=: num_redtiles.value) (a -: num_redtiles.value) a
  in

  let area64 = (find_area ~a:p_compute.value ~b:q_compute.value) -- "area" in

  let redtile_idx_compute = Variable.reg spec ~width:redtile_addr_w in
  let no_in_btw = Variable.reg spec ~width:1 in

  compile
    [
      ram_we <--. 0;
      ram_waddr <--. 0;
      ram_wdata <--. 0;

      p1_done <--. 0;
      p2_done <--. 0;

      sm.switch
        [
          (States.Idle,
            [
              when_ uart_rx.valid
                [
                  if_ (uart_rx.value ==:. 0x02)
                    [
                      sm.set_next States.Receive;
                    ][
                      sm.set_next States.Idle;
                    ]
                ];
            ]);
          (States.Receive,
            [
              when_ uart_rx.valid
                [
                  if_ (uart_rx.value ==:. 0x03)
                    [
                      when_ (coord_sel_receive.value ==:. 1)
                        [
                          proc (write_array x_coords ~idx:num_redtiles.value ~data:tmp_x_receive.value);
                          proc (write_array y_coords ~idx:num_redtiles.value ~data:cur_value_receive.value);
                          num_redtiles <-- (num_redtiles.value +:. 1);
                          tmp_x_receive <--. 0;
                          cur_value_receive <--. 0;
                          coord_sel_receive <--. 0;

                          ram_we <--. 1;
                          ram_waddr <-- num_redtiles.value;
                          ram_wdata <-- (pack_border ~x:tmp_x_receive.value ~y:cur_value_receive.value);
                        ];

                      p_compute <--. 0;
                      q_compute <--. 1;
                      ram_raddr0 <--. 0;
                      ram_raddr1 <--. 1;
                      redtile_idx_compute <--. 0;
                      no_in_btw <--. 1;
                      sm.set_next States.Compute;
                    ][
                      if_ (Math.is_digit uart_rx.value)
                        [
                          cur_value_receive <-- ((Math.mul10 cur_value_receive.value) +: (uresize (uart_rx.value -:. Char.to_int '0') 32));
                        ][
                          if_ (uart_rx.value ==:. Char.to_int ',')
                            [
                              when_ (coord_sel_receive.value ==:. 0) [ tmp_x_receive <-- cur_value_receive.value; ];
                              cur_value_receive <--. 0;
                              coord_sel_receive <-- (coord_sel_receive.value +:. 1);
                            ][
                              when_ (uart_rx.value ==:. Char.to_int '\n')
                                [
                                  proc (write_array x_coords ~idx:num_redtiles.value ~data:tmp_x_receive.value);
                                  proc (write_array y_coords ~idx:num_redtiles.value ~data:cur_value_receive.value);
                                  num_redtiles <-- (num_redtiles.value +:. 1);
                                  tmp_x_receive <--. 0;
                                  cur_value_receive <--. 0;
                                  coord_sel_receive <--. 0;

                                  ram_we <--. 1;
                                  ram_waddr <-- num_redtiles.value;
                                  ram_wdata <-- (pack_border ~x:tmp_x_receive.value ~y:cur_value_receive.value);
                                ];
                            ]
                        ]
                    ]
                ];
            ]);
          (States.Compute, (* try any 2-combination of redtiles, maximize area *)
            [
              if_ (p_compute.value +:. 1 ==: num_redtiles.value)
                [
                  sm.set_next States.Done;
                ][
                  when_ (area64 >: p1_result.value) [ p1_result <-- area64; ];

                  ram_raddr0 <--. 0;
                  ram_raddr1 <--. 1;
                  no_in_btw <--. 1;
                  redtile_idx_compute <--. 0;

                  if_ ((area64 >: p2_result.value) &: no_in_btw.value)
                    [
                      if_ (redtile_idx_compute.value ==: num_redtiles.value)
                        [
                          when_ (no_in_btw.value) [ p2_result <-- area64; ];
                          redtile_idx_compute <--. 1;
                          no_in_btw <--. 1;
                        ][
                          no_in_btw <-- (no_in_btw.value &: ~:(is_between ~a:p_compute.value ~b:q_compute.value ~q1:redtile0 ~q2:redtile1));
                          ram_raddr0 <-- (mod_num_redtiles (redtile_idx_compute.value +:. 1));
                          ram_raddr1 <-- (mod_num_redtiles (redtile_idx_compute.value +:. 2));
                          redtile_idx_compute <-- (redtile_idx_compute.value +:. 1);
                        ];
                    ][
                      if_ (q_compute.value +:. 1 ==: num_redtiles.value)
                        [
                          p_compute <-- (p_compute.value +:. 1);
                          q_compute <-- (p_compute.value +:. 2);
                        ][
                          q_compute <-- (q_compute.value +:. 1);
                        ]
                    ];
                ]
            ]);
          (States.Done,
            [
              done_ctr <-- (done_ctr.value +:. 1);
              when_ (done_ctr.value >=:. done_th)
                [
                  done_ctr <--. 0;
                  sm.set_next States.Idle;
                ];
            ]);
        ];
    ];

  (* waveform stuff *)
  let _uart_value = uart_rx.value -- "uart_value" in
  let _uart_valid = uart_rx.valid -- "uart_valid" in
  let p1_done_value = p1_done.value -- "p1_done" in
  let p2_done_value = p2_done.value -- "p2_done" in
  let cur_value_receive_value = cur_value_receive.value -- "cur_value_receive" in
  let ram_we_value = ram_we.value -- "ram_we" in
  let ram_waddr_value = ram_waddr.value -- "ram_waddr" in
  let ram_wdata_value = ram_wdata.value -- "ram_wdata" in
  let ram_wdata_x = unpack_border_x ram_wdata_value -- "ram_wdata_x" in
  let ram_wdata_y = unpack_border_y ram_wdata_value -- "ram_wdata_y" in
  let p_compute_value = p_compute.value -- "p_compute" in
  let q_compute_value = q_compute.value -- "q_compute" in
  let is_state_idle = sm.is States.Idle -- "is_state_idle" in
  let is_state_receive = sm.is States.Receive -- "is_state_receive" in
  let is_state_compute = sm.is States.Compute -- "is_state_compute" in
  let is_state_done = sm.is States.Done -- "is_state_done" in
  let num_redtiles_value = num_redtiles.value -- "num_redtiles" in
  let redtile_idx_compute_value = redtile_idx_compute.value -- "redtile_idx_compute" in
  let no_in_btw_value = no_in_btw.value -- "no_in_btw" in

  let debug_output =
    p1_done_value
    |: p2_done_value
    |: (cur_value_receive_value >:. 0)
    |: (ram_we_value >:. 0)
    |: (ram_waddr_value >:. 0)
    |: (ram_wdata_value >:. 0)
    |: (ram_wdata_x >:. 0)
    |: (ram_wdata_y >:. 0)
    |: (p_compute_value >:. 0)
    |: (q_compute_value >:. 0)
    |: is_state_idle
    |: is_state_receive
    |: is_state_compute
    |: is_state_done
    |: (num_redtiles_value >:. 0)
    |: (redtile_idx_compute_value >:. 0)
    |: (no_in_btw_value >:. 0)
  in

  p1_result.value, p2_result.value, (sm.is States.Done), debug_output
;;

let create ~clock ~clear ~cycles_per_bit ~max_redtiles uart_rx_value =
  let uart_rx = Uart.Expert.create_rx_state_machine
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in
  create_logic ~clock ~clear ~max_redtiles uart_rx
;;
