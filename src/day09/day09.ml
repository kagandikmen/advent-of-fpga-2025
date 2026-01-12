(*
 *
 * AoF - Hardcaml Solution for Day 9 (Step 1 & Step 2)
 * Created:     2026-01-02
 * Modified:    2026-01-12
 * Author:      Kagan Dikmen
 *
 *)

(* TODO: Add FPGA build and RTL generation logic *)

open! Core
open! Hardcaml
open! Hardcaml_aof
open! Hardcaml_arty
open! Signal

module States = struct
  type t =
    | Idle
    | Receive
    | Find_borders
    | Compute
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

let ( *^: ) a b = uresize (a *: b) (width a)

let abs a b = mux2 (a >=: b) (a -: b) (b -: a)

let read_array = Procedure.read_array
let write_array = Procedure.write_array

let create ~clock ~clear ~cycles_per_bit uart_rx_value =
  let open Always in

  let max_redtiles = 512 in
  let max_borders = 1 lsl 20 in

  let redtile_addr_w = Math.ceil_log2 max_redtiles in (* 9 *)
  let border_addr_w = Math.ceil_log2 max_borders in (* 9 *)

  let uart_rx = Uart.Expert.create_rx_state_machine
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in

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

  let ram_raddr = Variable.wire ~default:(zero border_addr_w) in
  let ram_waddr = Variable.wire ~default:(zero border_addr_w) in
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

  let read_port : Signal.read_port =
    {
      read_clock = clock;
      read_enable = vdd;
      read_address = ram_raddr.value;
    }
  in

  let ram_rdata = Ram.create
    ~collision_mode:Read_before_write
    ~size:max_borders
    ~write_ports:[| write_port |]
    ~read_ports:[| read_port; |]
    ()
  in

  let redtile0 = ram_rdata.(0) in

  let p_findborders = Variable.reg spec ~width:redtile_addr_w in
  let q_findborders = Variable.reg spec ~width:redtile_addr_w in

  let p_compute = Variable.reg spec ~width:redtile_addr_w in
  let q_compute = Variable.reg spec ~width:redtile_addr_w in
  
  let find_area ~(a: Signal.t) ~(b: Signal.t) =
    let xa = read_array x_coords ~idx:a in
    let ya = read_array y_coords ~idx:a in
    let xb = read_array x_coords ~idx:b in
    let yb = read_array y_coords ~idx:b in
    
    let dx = abs xa xb in
    let dy = abs ya yb in

    (dx +:. 1) *^: (dy +:. 1)
  in

  let is_between ~(a: Signal.t) ~(b: Signal.t) ~(q: Signal.t) =
    (* Is red tile addressed by q between red tiles addressed by a and b? *)
    let xa = read_array x_coords ~idx:a in
    let ya = read_array y_coords ~idx:a in
    let xb = read_array x_coords ~idx:b in
    let yb = read_array y_coords ~idx:b in
    let xq = unpack_border_x q in
    let yq = unpack_border_y q in

    let a_has_greater_x = xa >=: xb in
    let a_has_greater_y = ya >=: yb in

    let xq_in_between = mux2 a_has_greater_x ((xa >: xq) &: (xq >: xb)) ((xb >: xq) &: (xq >: xa)) in
    let yq_in_between = mux2 a_has_greater_y ((ya >: yq) &: (yq >: yb)) ((yb >: yq) &: (yq >: ya)) in

    xq_in_between &: yq_in_between
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

  let bordertile_ctr = Variable.reg spec ~width:border_addr_w in
  let line_ctr = Variable.reg spec ~width:32 in

  let px = (read_array x_coords ~idx:p_findborders.value) -- "px" in
  let py = (read_array y_coords ~idx:p_findborders.value) -- "py" in
  let qx = (read_array x_coords ~idx:q_findborders.value) -- "qx" in 
  let qy = (read_array y_coords ~idx:q_findborders.value) -- "qy" in
  let iter_y = mux2 (qy >=: py) (py +: line_ctr.value) (py -: line_ctr.value) in
  let iter_x = mux2 (qx >=: px) (px +: line_ctr.value) (px -: line_ctr.value) in

  let area = (find_area ~a:p_compute.value ~b:q_compute.value) -- "area" in
  let area64 = uresize area 64 in

  let border_ctr_compute = Variable.reg spec ~width:border_addr_w in
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
                      if_ (coord_sel_receive.value ==:. 1)
                        [
                          proc (write_array x_coords ~idx:num_redtiles.value ~data:tmp_x_receive.value);
                          proc (write_array y_coords ~idx:num_redtiles.value ~data:cur_value_receive.value);
                          num_redtiles <-- (num_redtiles.value +:. 1);
                          tmp_x_receive <--. 0;
                          cur_value_receive <--. 0;
                          coord_sel_receive <--. 0;
                        ][
                        ];

                      p_findborders <--. 0;
                      q_findborders <--. 1;
                      bordertile_ctr <--. 0;
                      line_ctr <--. 0;
                      sm.set_next States.Find_borders;
                    ][
                      if_ (Math.is_digit uart_rx.value)
                        [
                          cur_value_receive <-- ((Math.mul10 cur_value_receive.value) +: (uresize (uart_rx.value -:. Char.to_int '0') 32));
                        ][
                          if_ (uart_rx.value ==:. Char.to_int ',')
                            [
                              if_ (coord_sel_receive.value ==:. 0) [ tmp_x_receive <-- cur_value_receive.value; ][];
                              cur_value_receive <--. 0;
                              coord_sel_receive <-- (coord_sel_receive.value +:. 1);
                            ][
                              if_ (uart_rx.value ==:. Char.to_int '\n')
                                [
                                  proc (write_array x_coords ~idx:num_redtiles.value ~data:tmp_x_receive.value);
                                  proc (write_array y_coords ~idx:num_redtiles.value ~data:cur_value_receive.value);
                                  num_redtiles <-- (num_redtiles.value +:. 1);
                                  tmp_x_receive <--. 0;
                                  cur_value_receive <--. 0;
                                  coord_sel_receive <--. 0;
                                ][
                                ];
                            ]
                        ]
                    ]
                ];
            ]);
          (States.Find_borders, (* find border tiles, save them in ram *)
            [
              if_ (p_findborders.value ==: num_redtiles.value)
                [
                  p_compute <--. 0;
                  q_compute <--. 1;
                  ram_raddr <--. 0;
                  border_ctr_compute <--. 1;
                  no_in_btw <--. 1;
                  sm.set_next States.Compute;
                ][
                  if_ (px ==: qx) (* line is vertical *)
                    [
                      ram_we <--. 1;
                      ram_waddr <-- bordertile_ctr.value;
                      ram_wdata <-- (pack_border ~x:px ~y:iter_y);

                      bordertile_ctr <-- (bordertile_ctr.value +:. 1);
                      line_ctr <-- (line_ctr.value +:. 1);

                      when_ (iter_y ==: qy)
                        [
                          bordertile_ctr <-- (bordertile_ctr.value);
                          p_findborders <-- (p_findborders.value +:. 1);
                          q_findborders <-- (mod_num_redtiles (p_findborders.value +:. 2));
                          line_ctr <--. 0;
                        ];
                    ][
                      if_ (py ==: qy) (* line is horizontal *)
                        [
                          ram_we <--. 1;
                          ram_waddr <-- bordertile_ctr.value;
                          ram_wdata <-- (pack_border ~x:iter_x ~y:py);

                          bordertile_ctr <-- (bordertile_ctr.value +:. 1);
                          line_ctr <-- (line_ctr.value +:. 1);

                          when_ (iter_x ==: qx)
                            [
                              bordertile_ctr <-- (bordertile_ctr.value);
                              p_findborders <-- (p_findborders.value +:. 1);
                              q_findborders <-- (mod_num_redtiles (p_findborders.value +:. 2));
                              line_ctr <--. 0;
                            ]
                        ][
                          (* impossible *)
                        ];
                    ];
                  ]
            ]);
          (States.Compute, (* try any 2-combination of redtiles, maximize area *)
            [
              if_ (p_compute.value +:. 1 ==: num_redtiles.value)
                [
                  sm.set_next States.Done;
                ][
                  when_ (area64 >: p1_result.value)
                    [
                      p1_result <-- area64;
                    ];

                  ram_raddr <--. 0;
                  no_in_btw <--. 1;
                  border_ctr_compute <--. 1;

                  if_ ((area64 >: p2_result.value) &: no_in_btw.value)
                    [
                      if_ (border_ctr_compute.value ==: bordertile_ctr.value)
                        [
                          if_ (no_in_btw.value)
                            [
                              p2_result <-- area64;
                            ][
                            ];

                          border_ctr_compute <--. 1;
                          no_in_btw <--. 1;
                        ][
                          no_in_btw <-- (no_in_btw.value &: ~:(is_between ~a:p_compute.value ~b:q_compute.value ~q:redtile0));
                          ram_raddr <-- border_ctr_compute.value;
                          border_ctr_compute <-- (border_ctr_compute.value +:. 1);
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
  let p_findborders_value = p_findborders.value -- "p_findborders" in
  let q_findborders_value = q_findborders.value -- "q_findborders" in
  let p_compute_value = p_compute.value -- "p_compute" in
  let q_compute_value = q_compute.value -- "q_compute" in
  let is_state_idle = sm.is States.Idle -- "is_state_idle" in
  let is_state_receive = sm.is States.Receive -- "is_state_receive" in
  let is_state_fborders = sm.is States.Find_borders -- "is_state_fborders" in
  let is_state_compute = sm.is States.Compute -- "is_state_compute" in
  let is_state_done = sm.is States.Done -- "is_state_done" in
  let num_redtiles_value = num_redtiles.value -- "num_redtiles" in
  let bordertile_ctr_value = bordertile_ctr.value -- "bordertile_ctr" in 
  let border_ctr_compute_value = border_ctr_compute.value -- "border_ctr_compute" in

  let debug_output =
    p1_done_value
    |: p2_done_value
    |: (cur_value_receive_value >:. 0)
    |: (ram_we_value >:. 0)
    |: (ram_waddr_value >:. 0)
    |: (ram_wdata_value >:. 0)
    |: (ram_wdata_x >:. 0)
    |: (ram_wdata_y >:. 0)
    |: (p_findborders_value >:. 0)
    |: (q_findborders_value >:. 0)
    |: (p_compute_value >:. 0)
    |: (q_compute_value >:. 0)
    |: is_state_idle
    |: is_state_receive
    |: is_state_fborders
    |: is_state_compute
    |: is_state_done
    |: (num_redtiles_value >:. 0)
    |: (bordertile_ctr_value >:. 0)
    |: (border_ctr_compute_value >:. 0)
  in

  p1_result.value, p2_result.value, (sm.is States.Done), debug_output
;;
