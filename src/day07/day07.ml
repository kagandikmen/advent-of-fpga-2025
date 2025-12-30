(*
 *
 * AoF - Hardcaml Solution for Day 7 (Step 1 & Step 2)
 * Created:     2025-12-28
 * Modified:    2025-12-30
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
    | Compute
    | Switch_rows
    | Conclude
    | Done
  [@@deriving sexp_of, compare, enumerate]
end


let create_day07_logic ~clock ~clear ~cycles_per_bit uart_rx_value =
  let open Always in

  let max_rows = 256 in
  let max_cols = 256 in
  let grid_size = max_rows * max_cols in

  let uart_rx = Uart.Expert.create_rx_state_machine
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in

  let _uart_value = uart_rx.value -- "uart_value" in
  let _uart_valid = uart_rx.valid -- "uart_valid" in

  let spec = Reg_spec.create
    ~clock
    ~clear
    ()
  in

  let total_splits = Variable.reg spec ~width:64 in
  let total_paths = Variable.reg spec ~width:64 in

  let _total_splits_value = total_splits.value -- "total_splits_value" in
  let _total_paths_value = total_paths.value -- "total_paths_value" in

  let sm = State_machine.create (module States) spec in

  let row_addr_width = Math.ceil_log2 max_rows in
  let col_addr_width = Math.ceil_log2 max_cols in
  let addr_width = Math.ceil_log2 grid_size in

  let row = Variable.reg spec ~width:row_addr_width in (* current row idx *)
  let col = Variable.reg spec ~width:col_addr_width in (* current col idx *)
  let row_cnt = Variable.reg spec ~width:row_addr_width in (* #rows *)
  let col_cnt = Variable.reg spec ~width:col_addr_width in (* #cols *)

  let s_col = Variable.reg spec ~width:col_addr_width in

  let idx_last_row = row_cnt.value -:. 1 in
  let idx_last_col = col_cnt.value -:. 1 in

  let ram_raddr = Variable.wire ~default:(zero addr_width) in
  let ram_waddr = Variable.wire ~default:(zero addr_width) in
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

  let ram_read_port : Signal.read_port =
    {
      read_clock = clock;
      read_enable = vdd;
      read_address = ram_raddr.value;
    }
  in 

  let ram_rdata = Ram.create
    ~collision_mode:Read_before_write
    ~size:grid_size
    ~write_ports:[| ram_write_port; |]
    ~read_ports: [| ram_read_port; |]
    ()
  in

  let ram_rdata0 = ram_rdata.(0) in   (* ram_rdata is still an array *)

  let idx_into_grid ~r ~c =
    let prod = (uresize r addr_width) *: (Signal.of_int ~width:addr_width max_cols) in
    (uresize prod addr_width) +: (uresize c addr_width)
  in 

  let read_grid ~r ~c =
    ram_raddr <-- (idx_into_grid ~r ~c);
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

  let s_ascii = Char.to_int 'S' in
  let tick_ascii = Char.to_int '^' in
  let _dot_ascii = Char.to_int '.' in
  let nl_ascii = Char.to_int '\n' in

  let row_l = Array.init max_cols ~f:(fun _ -> Variable.reg spec ~width:64) in
  let row_n = Array.init max_cols ~f:(fun _ -> Variable.reg spec ~width:64) in

  let copy_array ~(target: Always.Variable.t array) ~(source: Always.Variable.t array) =
    if (Array.length target) <> (Array.length source) then failwith "Error 10001"
    else (
      List.init (Array.length target) ~f:(fun i ->
        target.(i) <-- source.(i).value;
        )
      )
  in

  let zero_out_array (target: Always.Variable.t array) =
    List.init (Array.length target) ~f:(fun i -> 
      target.(i) <--. 0;
    )
  in

  let add_to_array (target: Always.Variable.t array) ~(idx: Signal.t) ~(data: Signal.t) =
    let present = Procedure.read_array target ~idx in
    Procedure.write_array target ~idx ~data:(present +: data)
  in

  let done_ctr = Variable.reg spec ~width:8 in
  let done_th = 100 in

  let row_o = Variable.reg spec ~width:row_addr_width in
  let col_o = Variable.reg spec ~width:col_addr_width in
  
  let is_rdata_valid = Variable.reg spec ~width:1 in

  compile
    [
      ram_we <--. 0;
      ram_waddr <--. 0;
      ram_wdata <--. 0;

      sm.switch
        [
          (States.Idle,
            [
              when_ uart_rx.valid
                [
                  if_ (uart_rx.value ==:. 0x02) [ sm.set_next States.Receive; ][ sm.set_next States.Idle; ]
                ];
            ]);
          (States.Receive,
            [
              when_ uart_rx.valid
                [
                  if_ (uart_rx.value ==:. 0x03)
                    [
                      row_cnt <-- (row.value +:. 1);
                      
                      proc (zero_out_array row_l);
                      proc (zero_out_array row_n);

                      proc (Procedure.write_array row_l ~idx:s_col.value ~data:(one 64));

                      row <--. 1;
                      col <--. 0;

                      row_o <--. 0;
                      col_o <--. 0;

                      sm.set_next States.Compute;
                    ][
                      if_ (uart_rx.value ==:. nl_ascii)
                        [
                          when_ (col.value >: col_cnt.value) [ col_cnt <-- col.value; ];
                          row <-- (row.value +:. 1);
                          col <--. 0;
                        ][
                          proc (write_grid ~r:row.value ~c:col.value ~data:uart_rx.value);
                          col <-- (col.value +:. 1);
                          when_ (uart_rx.value ==:. s_ascii) [ s_col <-- col.value; ];
                        ]
                    ]
                ];
            ]);
          (States.Compute,
            let is_eor = Variable.wire ~default:gnd in  (* end of row *)
            [
              read_grid ~r:row.value ~c:col.value;

              when_ (is_rdata_valid.value ==:. 1)
                (
                  let q = Procedure.read_array row_l ~idx:col_o.value in
                  [
                    if_ (ram_rdata0 ==:. tick_ascii)
                      [
                        total_splits <-- (total_splits.value +: uresize (q >:. 0) 64);
                        when_ (col_o.value >:. 0) [ proc (add_to_array row_n ~idx:(col_o.value -:. 1) ~data:q); ];
                        when_ (col_o.value <: idx_last_col) [ proc (add_to_array row_n ~idx:(col_o.value +:. 1) ~data:q)];
                      ][
                        proc(add_to_array row_n ~idx:col_o.value ~data:q);
                      ];

                    when_ (col_o.value ==: idx_last_col)
                      [
                        is_eor <--. 1;
                        sm.set_next States.Switch_rows;
                      ]
                  ]
                );
              
              when_ (is_eor.value ==:. 0)
                [
                  is_rdata_valid <--. 1;

                  row_o <-- row.value;
                  col_o <-- col.value;

                  if_ (col.value ==: idx_last_col)
                    [ row <-- (row.value +:. 1); col <--. 0; ]
                    [ col <-- (col.value +:. 1); ];
                ]
            ]);
          (States.Switch_rows,
            let final_row_done = is_rdata_valid.value &: (row_o.value ==: idx_last_row) &: (col_o.value ==: idx_last_col) in
            [
              proc (copy_array ~target:row_l ~source:row_n);
              proc (zero_out_array row_n);

              if_ final_row_done
                [
                  total_paths <--. 0;
                  col <--. 0;
                  is_rdata_valid <--. 0;
                  sm.set_next States.Conclude;
                ][
                  row_o <-- (row.value +:. 1);
                  col_o <--. 0;
                  is_rdata_valid <--. 1;
                  sm.set_next States.Compute;

                  if_ (col.value ==: idx_last_col)
                    [ row <-- (row.value +:. 1); col <--. 0; ]
                    [ col <-- (col.value +:. 1); ];
                ]
            ]);
          (States.Conclude,
            let final_row_char = Procedure.read_array row_l ~idx:col.value in
            [
              total_paths <-- (total_paths.value +: final_row_char);

              if_ (col.value ==: idx_last_col) 
                [ sm.set_next States.Done; ]
                [ col <-- (col.value +:. 1); ]
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
        ]
    ];

  total_splits.value, total_paths.value, (sm.is States.Done)
;;
