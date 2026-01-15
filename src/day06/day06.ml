(*
 *
 * AoF - Hardcaml Solution for Day 6
 * Created:     2025-12-26
 * Modified:    2026-01-15
 * Author:      Kagan Dikmen
 *
 *)

(*************************** IMPORTANT ****************************)
(* For an older solution, see src/old/day06/                      *)
(******************************************************************)

open! Core
open! Hardcaml
open! Hardcaml_aof
open! Hardcaml_arty
open! Signal

module States = struct
  type t =
    | Idle
    | Receive
    | Find_opblk_start
    | Find_opblk_end
    | Setup_compute
    | Compute
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

module Secondary_states = struct
  type t =
    | Idle
    | Retrieve
    | Accumulate
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

let ( *^: ) (a: Signal.t) (b: Signal.t) : Signal.t = uresize (a *: b) 64

let create_logic ~clock ~clear ~max_rows ~max_cols (uart_rx: Signal.t Uart.Byte_with_valid.t) =
  let open Always in

  let grid_size = max_rows * max_cols in

  let _uart_value = uart_rx.value -- "uart_value" in
  let _uart_valid = uart_rx.valid -- "uart_valid" in

  let spec = Reg_spec.create
    ~clock
    ~clear
    ()
  in

  let total_p1 = Variable.reg spec ~width:64 in
  let total_p2 = Variable.reg spec ~width:64 in

  let _total_p1_value = total_p1.value -- "total_p1_value" in
  let _total_p2_value = total_p2.value -- "total_p2_value" in

  (* state machines *)
  let sm = State_machine.create (module States) spec in
  let p1_sm = State_machine.create (module Secondary_states) spec in
  let p2_sm = State_machine.create (module Secondary_states) spec in

  let row_addr_width = Math.ceil_log2 max_rows in
  let col_addr_width = Math.ceil_log2 max_cols in
  let idx_width = Math.ceil_log2 grid_size in

  let row = Variable.reg spec ~width:row_addr_width in
  let col = Variable.reg spec ~width:col_addr_width in
  let row_cnt = Variable.reg spec ~width:row_addr_width in
  let col_cnt_max = Variable.reg spec ~width:col_addr_width in

  let idx_last_row = row_cnt.value -:. 1 in
  let idx_last_data_row = row_cnt.value -:. 2 in

  let ram_raddr0 = Variable.wire ~default:(zero idx_width) in
  let ram_raddr1 = Variable.wire ~default:(zero idx_width) in
  let ram_waddr = Variable.wire ~default:(zero idx_width) in
  let ram_we = Variable.wire ~default:gnd in
  let ram_wdata = Variable.wire ~default:(zero 8) in

  let ram_write_port : Signal.write_port =
    {
      write_clock = clock;
      write_enable = ram_we.value;
      write_address = ram_waddr.value;
      write_data = ram_wdata.value;
    }
  in

  let ram_read_port_0 : Signal.read_port =
    {
      read_clock = clock;
      read_enable = vdd;
      read_address = ram_raddr0.value;
    }
  in

  let ram_read_port_1 : Signal.read_port =
    {
      read_clock = clock;
      read_enable = vdd;
      read_address = ram_raddr1.value;
    }
  in

  (* ram *)
  let ram_rdata = Ram.create
    ~collision_mode:Read_before_write
    ~size:grid_size
    ~write_ports:[| ram_write_port |]
    ~read_ports:[| ram_read_port_0; ram_read_port_1; |]
    ()
  in

  let ram_rdata0 = ram_rdata.(0) in
  let ram_rdata1 = ram_rdata.(1) in

  let idx_into_grid ~r ~c =
    let prod = (uresize r idx_width) *: (Signal.of_int ~width:idx_width max_cols) in
    (uresize prod idx_width) +: (uresize c idx_width)
  in

  let space_sig = Signal.of_int ~width:8 (Char.to_int ' ') in

  let read_grid0 ~r ~c =
      ram_raddr0 <-- (idx_into_grid ~r ~c);
  in

  let read_grid1 ~r ~c =
      ram_raddr1 <-- (idx_into_grid ~r ~c);
  in

  let write_ram ~(addr: Signal.t) ~(data: Signal.t) =
    [
      ram_we <--. 1;
      ram_waddr <-- addr;
      ram_wdata <-- data;
    ]
  in

  let write_grid ~r ~c ~data = 
    write_ram ~addr:(idx_into_grid ~r ~c) ~data
  in

  let col_current = Variable.reg spec ~width:col_addr_width in
  let opblk_start = Variable.reg spec ~width:col_addr_width in
  let opblk_end = Variable.reg spec ~width:col_addr_width in

  let is_op_plus = Variable.reg spec ~width:1 in
  let is_op_mul = Variable.reg spec ~width:1 in

  let p1_start = Variable.reg spec ~width:1 in
  let p1_done = Variable.reg spec ~width:1 in
  let p1_row = Variable.reg spec ~width:row_addr_width in
  let p1_col = Variable.reg spec ~width:col_addr_width in
  let p1_acc = Variable.reg spec ~width:64 in
  let p1_cur = Variable.reg spec ~width:64 in

  let p2_start = Variable.reg spec ~width:1 in
  let p2_done = Variable.reg spec ~width:1 in
  let p2_row = Variable.reg spec ~width:row_addr_width in
  let p2_col = Variable.reg spec ~width:col_addr_width in
  let p2_acc = Variable.reg spec ~width:64 in
  let p2_cur = Variable.reg spec ~width:64 in

  let plus_ascii = Char.to_int '+' in
  let star_ascii = Char.to_int '*' in

  let is_col_filled = Array.init max_cols ~f:(fun _ -> Variable.reg spec ~width:1) in 

  compile
    [
      p1_start <--. 0;
      p2_start <--. 0;

      ram_we <--. 0;
      ram_waddr <--. 0;
      ram_wdata <--. 0;

      ram_raddr0 <--. 0;
      ram_raddr1 <--. 0;

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
                      row_cnt <-- (row.value +:. 1);
                      col_current <--. 0;
                      sm.set_next States.Find_opblk_start;
                    ][
                      if_ (uart_rx.value ==:. Char.to_int '\n')
                        [
                          when_ (col.value >: col_cnt_max.value)
                            [
                              col_cnt_max <-- col.value;
                            ];
                          row <-- (row.value +:. 1);
                          col <--. 0;
                        ](
                          write_grid ~r:row.value ~c:col.value ~data:uart_rx.value
                          @ [
                            let is_ns =
                              (~:(uart_rx.value ==:. Char.to_int ' '))
                              &: (~:(uart_rx.value ==:. 0))
                            in
                            when_ is_ns
                              (Procedure.write_array is_col_filled ~idx:col.value ~data:vdd)
                          ]
                          @ [
                            col <-- (col.value +:. 1)
                          ]
                        )
                    ]
                ];
            ]);
          (States.Find_opblk_start,
            [
              if_ (col_current.value ==: col_cnt_max.value)
                [
                  sm.set_next States.Done
                ][
                  let is_col_current_filled = Procedure.read_array is_col_filled ~idx:col_current.value in
                  if_ (is_col_current_filled ==:. 0)
                    [ col_current <-- (col_current.value +:. 1) ]
                    [
                      opblk_start <-- col_current.value;
                      sm.set_next States.Find_opblk_end;
                    ]
                ]
            ]);
          (States.Find_opblk_end,
            [
              read_grid0 ~r:idx_last_row ~c:opblk_start.value;

              if_ (col_current.value ==: col_cnt_max.value)
                [
                  opblk_end <-- col_cnt_max.value;
                  sm.set_next States.Setup_compute;
                ][
                  let is_col_current_filled = Procedure.read_array is_col_filled ~idx:col_current.value in
                  if_ (is_col_current_filled ==:. 0)
                    [
                      opblk_end <-- col_current.value;
                      sm.set_next States.Setup_compute;
                    ][
                      col_current <-- (col_current.value +:. 1);
                    ]
                ]
            ]);
          (States.Setup_compute,
            let op = mux2 (ram_rdata0 ==:. 0) space_sig ram_rdata0 in
            [
              is_op_plus <--. 0;
              is_op_mul <--. 0;
              when_ (op ==:. plus_ascii)
                [
                  is_op_plus <--. 1;
                ];
              when_ (op ==:. star_ascii)
                [
                  is_op_mul <--. 1;
                ];
              p1_done <--. 0;
              p2_done <--. 0;
              p1_start <--. 1;
              p2_start <--. 1;
              sm.set_next States.Compute;
            ]);
          (States.Compute,
            [
              when_ (p1_done.value &: p2_done.value)
                [
                  col_current <-- opblk_end.value;
                  sm.set_next States.Find_opblk_start;
                ];
            ]);
          (States.Done, []);
        ];

      p1_sm.switch
        [
          (Secondary_states.Idle,
            [
              when_ (p1_start.value ==:. 1)
                [
                  p1_row <--. 0;
                  p1_col <-- opblk_start.value;
                  p1_acc <-- mux2 is_op_plus.value (zero 64) (one 64);
                  p1_cur <--. 0;
                  read_grid0 ~r:(zero row_addr_width) ~c:opblk_start.value;
                  p1_sm.set_next Secondary_states.Retrieve;
                ];
            ]);
          (Secondary_states.Retrieve,
            let chr = mux2 (ram_rdata0 ==:. 0) space_sig ram_rdata0 in
            [
              when_ (Math.is_digit chr)
                [
                  p1_cur <-- (Math.mul10 p1_cur.value +: uresize (Math.ascii_to_int8 chr) 64);
                ];
              
              if_ (p1_col.value +:. 1 ==: opblk_end.value)
                [
                  p1_sm.set_next Secondary_states.Accumulate;
                ][
                  p1_col <-- (p1_col.value +:. 1);
                  read_grid0 ~r:p1_row.value ~c:(p1_col.value +:. 1);
                ]
            ]);
          (Secondary_states.Accumulate,
            [
              p1_acc <--  mux2 is_op_plus.value
                            (p1_acc.value +: p1_cur.value)
                            (p1_acc.value *^: p1_cur.value);
              
              if_ (p1_row.value ==: idx_last_data_row)
                [
                  p1_sm.set_next Secondary_states.Done;
                ][
                  p1_row <-- (p1_row.value +:. 1);
                  p1_col <-- opblk_start.value;
                  p1_cur <--. 0;
                  read_grid0 ~r:(p1_row.value +:. 1) ~c:opblk_start.value;
                  p1_sm.set_next Secondary_states.Retrieve;
                ]
            ]);
          (Secondary_states.Done,
            [
              total_p1 <-- (total_p1.value +: p1_acc.value);
              p1_done <--. 1;
              p1_sm.set_next Secondary_states.Idle;
            ]);
        ];

      p2_sm.switch
        [
          (Secondary_states.Idle,
            [
              when_ (p2_start.value ==:. 1)
                [
                  p2_row <--. 0;
                  p2_col <-- opblk_end.value -:. 1;
                  p2_acc <-- mux2 is_op_plus.value (zero 64) (one 64);
                  p2_cur <--. 0;
                  read_grid1 ~r:(zero row_addr_width) ~c:(opblk_end.value -:. 1);
                  p2_sm.set_next Secondary_states.Retrieve;
                ];
            ]);
          (Secondary_states.Retrieve,
            let chr = mux2 (ram_rdata1 ==:. 0) space_sig ram_rdata1 in
            [
              when_ (Math.is_digit chr)
                [
                  p2_cur <-- (Math.mul10 p2_cur.value +: uresize (Math.ascii_to_int8 chr) 64);
                ];

              if_ (p2_row.value >=: idx_last_data_row)
                [
                  p2_sm.set_next Secondary_states.Accumulate;
                ][
                  p2_row <-- (p2_row.value +:. 1);
                  read_grid1 ~r:(p2_row.value +:. 1) ~c:p2_col.value;
                ]
            ]);
          (Secondary_states.Accumulate,
            [
              p2_acc <--  mux2 is_op_plus.value
                            (p2_acc.value +: p2_cur.value)
                            (p2_acc.value *^: p2_cur.value);
              
              if_ (p2_col.value ==: opblk_start.value)
                [
                  p2_sm.set_next Secondary_states.Done;
                ][
                  p2_row <--. 0;
                  p2_col <-- (p2_col.value -:. 1);
                  p2_cur <--. 0;
                  read_grid1 ~r:(zero row_addr_width) ~c:(p2_col.value -:. 1);
                  p2_sm.set_next Secondary_states.Retrieve;
                ]
            ]);
          (Secondary_states.Done,
            [
              total_p2 <-- (total_p2.value +: p2_acc.value);
              p2_done <--. 1;
              p2_sm.set_next Secondary_states.Idle;
            ]);
        ];
    ];
  

  total_p1.value, total_p2.value, (sm.is States.Done)
;;

let create ~clock ~clear ~cycles_per_bit ~max_rows ~max_cols uart_rx_value =
  let uart_rx = Uart.Expert.create_rx_state_machine
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in
  create_logic ~clock ~clear ~max_rows ~max_cols uart_rx
;;

