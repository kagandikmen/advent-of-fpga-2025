(*
 *
 * AoF - Hardcaml Solution for Day 1
 * Created:     2026-01-10
 * Modified:    2026-01-12
 * Author:      Kagan Dikmen
 *
 *)

(*************************** IMPORTANT ****************************)
(* For an older solution, see src/old/day01/                      *)
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
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

module Compute_states = struct
  type t =
    | Idle
    | Turn
  [@@deriving sexp_of, compare, enumerate]
end

let abs (x: Signal.t) =
  let is_lt0 = x <+. 0 in
  mux2 is_lt0 ((zero (width x)) -: x) x
;;

let divmod100 (d : Signal.t) : Signal.t * Signal.t =
  let step (q, r) =
    let is_ge100 = r >=:. 100 in
    let q' = mux2 is_ge100 (q +:. 1) q in
    let r' = mux2 is_ge100 (r -:. 100) r in
    q', r'
  in
  
  List.fold (List.init 9 ~f:Fn.id)
    ~init:(zero (width d), d)
    ~f:(fun acc _ -> step acc)
;;

let create_logic ~clock ~clear (uart_rx: Signal.t Uart.Byte_with_valid.t) =
  let open Always in

  let fifo_depth = 4 in (* subject to change come back to this *)

  let spec = Reg_spec.create
    ~clock
    ~clear
    ()
  in

  let sm = State_machine.create (module States) spec in
  let csm = State_machine.create (module Compute_states) spec in

  let stopped_at_zero_ctr = Variable.reg spec ~width:32 in
  let hit_zero_ctr = Variable.reg spec ~width:32 in

  let fifo_wr_en = Variable.wire ~default:gnd in
  let fifo_wr_data = Variable.wire ~default:(zero 16) in
  let fifo_rd_en = Variable.wire ~default:gnd in

  let fifo_rd_data, fifo_empty, _fifo_full = Fifo.create
    ~clock
    ~clear
    ~depth:fifo_depth
    ~width:16
    ~wr_en:fifo_wr_en.value
    ~wr_data:fifo_wr_data.value
    ~rd_en:fifo_rd_en.value
  in

  let is_etx_received = Variable.reg spec ~width:1 in

  let cur_value_receive = Variable.reg spec ~width:16 in
  let cur_value_receive_positive = Variable.reg spec ~width:1 in

  let cur_knob_position = Variable.reg spec ~width:20 in

  let done_ctr = Variable.reg spec ~width:8 in
  let done_th = 100 in

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
                  if_ (uart_rx.value ==:. 0x02)
                    [
                      is_etx_received <--. 0;

                      stopped_at_zero_ctr <--. 0;
                      hit_zero_ctr <--. 0;

                      cur_value_receive <--. 0;
                      cur_knob_position <--. 50;

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
                      is_etx_received <--. 1;

                      fifo_wr_en <--. 1;
                      if_ (cur_value_receive_positive.value ==:. 1) [ fifo_wr_data <-- cur_value_receive.value; ][ fifo_wr_data <-- ((zero 16) -: cur_value_receive.value); ];
                      cur_value_receive <--. 0;

                      sm.set_next States.Compute;
                    ][
                      when_ (uart_rx.value ==:. Char.to_int 'R')
                        [
                          cur_value_receive_positive <--. 1;
                        ];

                      when_ (uart_rx.value ==:. Char.to_int 'L')
                        [
                          cur_value_receive_positive <--. 0;
                        ];

                      when_ (uart_rx.value ==:. Char.to_int '\n')
                        [
                          fifo_wr_en <--. 1;
                          if_ (cur_value_receive_positive.value ==:. 1) [ fifo_wr_data <-- cur_value_receive.value; ][ fifo_wr_data <-- ((zero 16) -: cur_value_receive.value); ];
                          cur_value_receive <--. 0;
                        ];

                      when_ (Math.is_digit uart_rx.value)
                        (
                          let incoming_digit = Math.ascii_to_int8 uart_rx.value in
                          [
                            cur_value_receive <-- (Math.mul10 cur_value_receive.value +: uresize incoming_digit 16);
                          ]
                        );
                    ];
                ];
            ]);
          (States.Compute,
            [
              when_ (is_etx_received.value &: fifo_empty &: (csm.is Compute_states.Idle))
                [ sm.set_next States.Done; ]
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
                  csm.set_next Compute_states.Turn;
                ]
            ]);
          (Compute_states.Turn,
            let turn_mag = abs fifo_rd_data in
            let is_turn_left = msb fifo_rd_data in
            let wrap_eff, turn_eff = divmod100 turn_mag in

            let turn_knob pos_old turn_mag is_turn_dir_left =
              let raw = mux2 is_turn_dir_left (pos_old -: turn_mag) (pos_old +: turn_mag) in

              let is_raw_neg = msb raw in
              let is_raw_ge100 = raw >=:. 100 in

              let final = mux2 is_raw_neg (raw +:. 100) (mux2 is_raw_ge100 (raw -:. 100) raw) in

              let hundred = Signal.of_int ~width:(width pos_old) 100 in
              let d_to_zero = mux2 is_turn_dir_left pos_old (hundred -: pos_old) in
              let hit_zero = mux2 (pos_old ==:. 0) (zero 1) (mux2 (turn_mag >=: d_to_zero) (one 1) (zero 1)) in
              hit_zero, final
            in

            let hit_zero_this_time, final = turn_knob cur_knob_position.value (uresize turn_eff 20) is_turn_left in
            [
              cur_knob_position <-- final;
              hit_zero_ctr <-- (hit_zero_ctr.value +: (uresize hit_zero_this_time 32) +: (uresize wrap_eff 32));
              when_ (final ==:. 0) [stopped_at_zero_ctr <-- (stopped_at_zero_ctr.value +:. 1); ];

              csm.set_next Compute_states.Idle;
            ]);
        ];
    ];

  let uart_value = uart_rx.value -- "uart_value" in
  let uart_valid = uart_rx.valid -- "uart_valid" in
  let is_sm_idle = (sm.is States.Idle) -- "is_sm_idle" in
  let is_sm_receive = (sm.is States.Receive) -- "is_sm_receive" in
  let is_sm_compute = (sm.is States.Compute) -- "is_sm_compute" in
  let is_sm_done = (sm.is States.Done) -- "is_sm_done" in
  let is_csm_idle = (csm.is Compute_states.Idle) -- "is_csm_idle" in
  let is_csm_turn = (csm.is Compute_states.Turn) -- "is_csm_turn" in
  let fifo_wr_en_value = fifo_wr_en.value -- "fifo_wr_en" in
  let fifo_rd_en_value = fifo_rd_en.value -- "fifo_rd_en" in
  let fifo_wr_data_value = fifo_wr_data.value -- "fifo_wr_data" in
  let is_etx_received_value = is_etx_received.value -- "is_etx_received" in
  let cur_value_receive_value = cur_value_receive.value -- "cur_value_receive" in
  let cur_value_receive_positive_value = cur_value_receive_positive.value -- "cur_value_receive_positive" in
  let cur_knob_position_value = cur_knob_position.value -- "cur_knob_position" in

  let debug_output =
    concat_msb
      [
        uart_value;
        uart_valid;
        is_sm_idle;
        is_sm_receive;
        is_sm_compute;
        is_sm_done;
        is_csm_idle;
        is_csm_turn;
        fifo_wr_en_value;
        fifo_rd_en_value;
        fifo_wr_data_value;
        is_etx_received_value;
        cur_value_receive_value;
        cur_value_receive_positive_value;
        cur_knob_position_value;
      ];
  in

  stopped_at_zero_ctr.value, hit_zero_ctr.value, (sm.is States.Done), debug_output
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
