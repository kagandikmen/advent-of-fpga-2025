(*
 *
 * AoF - Hardcaml Solution for Day 7
 * Created:     2025-12-28
 * Modified:    2026-01-13
 * Author:      Kagan Dikmen
 *
 *)

(*************************** IMPORTANT ****************************)
(* For an older solution, see src/old/day07/                      *)
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
    | Compute
    | Conclude
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

module Compute_states = struct
  type t =
    | Idle
    | Compute_row
  [@@deriving sexp_of, compare, enumerate]
end

let create_logic ~clock ~clear (uart_rx: Signal.t Uart.Byte_with_valid.t) =
  let open Always in

  let fifo_depth = 2 in

  let spec = Reg_spec.create
    ~clock
    ~clear
    ()
  in

  let total_splits = Variable.reg spec ~width:64 in
  let total_paths = Variable.reg spec ~width:64 in

  let sm = State_machine.create (module States) spec in
  let csm = State_machine.create (module Compute_states) spec in

  let fifo_wr_en = Variable.wire ~default:gnd in
  let fifo_wr_data = Variable.wire ~default:(zero 256) in
  let fifo_rd_en = Variable.wire ~default:gnd in

  let fifo_rd_data, fifo_empty, _fifo_full = Fifo.create
    ~clock
    ~clear
    ~depth:fifo_depth
    ~width:256
    ~wr_en:fifo_wr_en.value
    ~wr_data:fifo_wr_data.value
    ~rd_en:fifo_rd_en.value
  in

  let done_ctr = Variable.reg spec ~width:8 in
  let done_th = 100 in

  let is_etx_received = Variable.reg spec ~width:1 in

  let cur_receive = Variable.reg spec ~width:256 in
  let rcv_first_row = Variable.reg spec ~width:1 in
  let rcv_idx = Variable.reg spec ~width:8 in

  let cur_rays = Array.init 256 ~f:(fun _ -> Variable.reg spec ~width:64) in

  let zero_out_array (target: Always.Variable.t array) =
    List.init (Array.length target) ~f:(fun i -> 
      target.(i) <--. 0;
    )
  in

  let add_to_array (target: Always.Variable.t array) ~(idx: Signal.t) ~(data: Signal.t) =
    let present = Procedure.read_array target ~idx in
    Procedure.write_array target ~idx ~data:(present +: data)
  in

  compile
    [
      fifo_wr_en <--. 0;
      fifo_rd_en <--. 0;
      fifo_wr_data <--. 0;

      sm.switch
        [
          (States.Idle,
            [
              when_ uart_rx.valid
                [
                  when_ (uart_rx.value ==:. 0x02) 
                    [ 
                      is_etx_received <--. 0;

                      proc (zero_out_array cur_rays);

                      total_splits <--. 0;
                      total_paths <--. 0;
                      cur_receive <--. 0;
                      rcv_first_row <--. 1;
                      rcv_idx <--. 0;

                      sm.set_next States.Receive; 
                    ];
                ];
            ]);
          (States.Receive,
            [
              when_ uart_rx.valid
                [
                  if_ (uart_rx.value ==:. 0x03)
                    [
                      is_etx_received <--. 1;

                      fifo_wr_en <--. 1;
                      fifo_wr_data <-- cur_receive.value;

                      sm.set_next States.Compute;
                    ][
                      if_ rcv_first_row.value
                        [
                          when_ (uart_rx.value ==:. Char.to_int 'S')
                            [
                              proc (add_to_array cur_rays ~idx:rcv_idx.value ~data:(one 64));
                              rcv_first_row <--. 0;
                            ];
                          
                          rcv_idx <-- (rcv_idx.value +:. 1);
                        ][
                          if_ (uart_rx.value ==:. Char.to_int '^')
                            [
                              cur_receive <-- ((select cur_receive.value 254 0) @: one 1);
                            ][
                              cur_receive <-- ((select cur_receive.value 254 0) @: zero 1);
                            ];
                          
                          when_ (uart_rx.value ==:. Char.to_int '\n')
                            [
                              fifo_wr_en <--. 1;
                              fifo_wr_data <-- cur_receive.value;
                              cur_receive <--. 0;
                            ];
                        ];
                    ];
                ];
            ]);
          (States.Compute,
            [
              when_ (is_etx_received.value &: fifo_empty &: (csm.is Compute_states.Idle))
                [ sm.set_next States.Conclude; ]
            ]);
          (States.Conclude,
            let num_paths =
              cur_rays
              |> Array.to_list
              |> List.map ~f:(fun v -> v.value)
              |> List.fold ~init:(zero 64) ~f:(+:)
            in
            [
              total_paths <-- num_paths;
              sm.set_next States.Done;
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

      csm.switch
        [
          (Compute_states.Idle,
            [
              when_ (~:fifo_empty)
                [
                  fifo_rd_en <--. 1;
                  csm.set_next Compute_states.Compute_row;
                ]
            ]);
          (Compute_states.Compute_row,
            let cur_row = fifo_rd_data in

            let cur_ray_value i = cur_rays.(i).value in
            let is_splitter i = bit cur_row i in

            let next_rays =
              Array.init 256 ~f:(fun i ->
                let self =
                  mux2 (is_splitter i) (zero 64) (cur_ray_value i)
                in
                let from_left =
                  if i = 0 then (zero 64)
                  else mux2 (is_splitter (i - 1)) (cur_ray_value (i - 1)) (zero 64)
                in
                let from_right =
                  if i = 255 then (zero 64)
                  else mux2 (is_splitter (i + 1)) (cur_ray_value (i + 1)) (zero 64)
                in
                self +: from_left +: from_right)
            in

            let split_events =
              List.init 256 ~f:Fn.id
              |> List.map ~f:(fun i -> mux2 ((cur_ray_value i >:. 0) &: is_splitter i) (one 64) (zero 64))
              |> List.fold ~init:(zero 64) ~f:(+:)
            in
            [
              proc (
                List.init 256 ~f:(fun i ->
                  cur_rays.(i) <-- next_rays.(i)
                )
              );

              total_splits <-- (total_splits.value +: split_events);
              csm.set_next Compute_states.Idle;
            ]);
        ];
    ];

  (* waveform stuff *)
  let uart_value = uart_rx.value -- "uart_value" in
  let uart_valid = uart_rx.valid -- "uart_valid" in
  let fifo_empty_value = fifo_empty -- "fifo_empty" in

  let debug_output =
    concat_msb
      [
        uart_value;
        uart_valid;
        fifo_empty_value;
      ];
  in

  total_splits.value, total_paths.value, (sm.is States.Done), debug_output
;;

let create ~clock ~clear ~cycles_per_bit uart_rx_value =
  let uart_rx = Uart.Expert.create_rx_state_machine
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in
  create_logic ~clock ~clear uart_rx
;;
