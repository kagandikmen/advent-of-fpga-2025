(*
 *
 * AoF - Hardcaml Solution for Day 2 (Step 1 & Step 2)
 * Created:     2026-01-07
 * Modified:    2026-01-10
 * Author:      Kagan Dikmen
 *
 *)

(*************************** IMPORTANT ****************************)
(* This is the newer & much-faster implementation.                *)
(* For an older solution, see day02_old.ml                        *)
(******************************************************************)

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
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

module Compute_states = struct
  type t =
    | Idle
    | Read
    | Init
    | Evaluate
    | Next
  [@@deriving sexp_of, compare, enumerate]
end

module Case = struct
  type t = {
    m: int;
    r: int;
    add_p1: bool;
    add_p2: bool;
  }

  let create ~m ~r ~add_p1 ~add_p2 =
    { m; r; add_p1; add_p2; }
end

let u64 x = uresize x 64
let ceil_log2 = Math.ceil_log2

module Fifo_40 = struct

  let create ~clock ~clear ~depth ~(wr_en: Signal.t) ~(wr_data: Signal.t) ~(rd_en: Signal.t) =
    let open Always in

    let spec = Reg_spec.create ~clock ~clear () in

    let addr_w = ceil_log2 depth in
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
    let wdata = Variable.wire ~default:(zero 40) in

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
end

let create_addition_logic ~clock ~clear ~cycles_per_bit uart_rx_value =
  let open Always in

  let fifo_depth = 64 in

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

  let num_silly_numbers = Variable.reg spec ~width:64 in
  let num_goofy_numbers = Variable.reg spec ~width:64 in

  let sm = State_machine.create (module States) spec in
  let csm = State_machine.create (module Compute_states) spec in

  let fifo_lo_wr_en = Variable.wire ~default:gnd in
  let fifo_lo_wr_data = Variable.wire ~default:(zero 40) in
  let fifo_lo_rd_en = Variable.wire ~default:gnd in

  let fifo_hi_wr_en = Variable.wire ~default:gnd in
  let fifo_hi_wr_data = Variable.wire ~default:(zero 40) in
  let fifo_hi_rd_en = Variable.wire ~default:gnd in

  let fifo_lo_rd_data, fifo_lo_empty, fifo_lo_full = Fifo_40.create
    ~clock
    ~clear
    ~depth:fifo_depth
    ~wr_en:fifo_lo_wr_en.value
    ~wr_data:fifo_lo_wr_data.value
    ~rd_en:fifo_lo_rd_en.value
  in

  let fifo_hi_rd_data, fifo_hi_empty, fifo_hi_full = Fifo_40.create
    ~clock
    ~clear
    ~depth:fifo_depth
    ~wr_en:fifo_hi_wr_en.value
    ~wr_data:fifo_hi_wr_data.value
    ~rd_en:fifo_hi_rd_en.value
  in

  let fifos_empty = (fifo_lo_empty &: fifo_hi_empty) -- "fifos_empty" in
  let fifos_full = (fifo_lo_full &: fifo_hi_full) -- "fifos_full" in

  let cur_bcd10 = Variable.reg spec ~width:40 in
  let lo_bcd10 = Variable.reg spec ~width:40 in

  let is_etx_received = Variable.reg spec ~width:1 in

  let done_ctr = Variable.reg spec ~width:8 in
  let done_th = 100 in

  let range_lo = Variable.reg spec ~width:40 in
  let range_hi = Variable.reg spec ~width:40 in
  let case_idx = Variable.reg spec ~width:6 in

  let pattern = Variable.reg spec ~width:40 in

  let m_r = Variable.reg spec ~width:4 in
  let r_r = Variable.reg spec ~width:4 in
  let add1_r = Variable.reg spec ~width:1 in
  let add2_r = Variable.reg spec ~width:1 in

  let xmin_r = Variable.reg spec ~width:40 in
  let xmax_r = Variable.reg spec ~width:40 in

  let bcd_digit_select (bcd: Signal.t) (i: int) : Signal.t =
    let lo = i * 4 in
    let hi = lo + 3 in
    select bcd hi lo
  in

  let bcd_le (a: Signal.t) (b: Signal.t) : Signal.t =
    let rec loop i =
      if i = 0 then vdd
      else
        let d_a = bcd_digit_select a (i-1) in
        let d_b = bcd_digit_select b (i-1) in
        mux2 (d_a <: d_b) vdd (mux2 (d_a >: d_b) gnd (mux2 (d_a ==: d_b) (loop (i-1)) gnd))
    in
    loop 10
  in

  let bcd_in_range ~lo ~hi ~x =
    bcd_le lo x &: bcd_le x hi
  in

  let bcd_to_bin (bcd: Signal.t) : Signal.t =
    let acc = ref (zero 40) in
    for i = 9 downto 0 do
      let d = bcd_digit_select bcd i in
      acc := uresize (Math.mul10 (!acc)) 40 +: uresize d 40
    done;
    !acc
  in

  let bcd_inc_slice ~digits (x_slice: Signal.t) : Signal.t =
    if digits = 0 then x_slice
    else
      let rec loop i carry acc =
        if i = digits then concat_msb acc
        else
          let d = select x_slice (i*4+3) (i*4) in
          let sum = uresize d 5 +: uresize carry 5 in
          let ge10 = sum >=:. 10 in
          let d' = mux2 ge10 (uresize (sum -:. 10) 4) (uresize sum 4) in
          let carry' = mux2 ge10 vdd gnd in
          loop (i+1) carry' (d' :: acc)
      in
      loop 0 vdd []
  in

  let xmin_bcd_const (m: int) : Int64.t =
    if m = 1 then 1L else Int64.shift_left 1L (4 * (m-1))
  in

  let xmax_bcd_const (m: int) : Int64.t =
    let rec loop k acc =
      if k = m then acc
      else
        let sh = 4 * k in
        loop (k+1) Int64.(acc lor (shift_left 9L sh))
    in
    loop 0 0L
  in

  (* array of all cases: (m=1,r=2), (m=1,r=3), ... *)
  let cases: Case.t array =
    let p1_cases = 
      [1; 2; 3; 4; 5;]
      |> List.map ~f:(fun m -> Case.create ~m ~r:2 ~add_p1:true ~add_p2:true)
    in

    let p2_cases = List.concat_map (List.range 1 11) ~f:(fun m -> 
      let max_r = 10 / m in
      List.range 2 (max_r + 1)
      |> List.filter_map ~f:(fun r -> 
        if r = 2 && m <= 5 then None
        else Some (Case.create ~m ~r ~add_p1:false ~add_p2:true)))
    in
    
    p1_cases @ p2_cases
      |> Array.of_list
  in

  let num_cases = Array.length cases in

  let case_m (case_idx: Signal.t) : Signal.t =
    Array.init num_cases ~f:(fun k -> 
      let m = cases.(k).m in
      (case_idx ==:. k, Signal.of_int ~width:4 m))
    |> Array.to_list
    |> List.fold ~init:(Signal.of_int ~width:4 0) ~f:(fun acc (sel,v) -> mux2 sel v acc)
  in

  let case_r (case_idx: Signal.t) : Signal.t =
    Array.init num_cases ~f:(fun k -> 
      let r = cases.(k).r in
      (case_idx ==:. k, Signal.of_int ~width:4 r))
    |> Array.to_list
    |> List.fold ~init:(Signal.of_int ~width:4 0) ~f:(fun acc (sel,v) -> mux2 sel v acc)
  in

  let case_add1 (case_idx: Signal.t) : Signal.t =
    Array.init num_cases ~f:(fun k -> 
      let v = if cases.(k).add_p1 then vdd else gnd in
      (case_idx ==:. k, v))
    |> Array.to_list
    |> List.fold ~init:gnd ~f:(fun acc (sel,v) -> mux2 sel v acc)
  in

  let case_add2 (case_idx: Signal.t) : Signal.t =
    Array.init num_cases ~f:(fun k -> 
      let v = if cases.(k).add_p2 then vdd else gnd in
      (case_idx ==:. k, v))
    |> Array.to_list
    |> List.fold ~init:gnd ~f:(fun acc (sel,v) -> mux2 sel v acc)
  in

  let case_xmin (case_idx: Signal.t) : Signal.t =
    Array.init num_cases ~f:(fun k ->
      let m = cases.(k).m in
      (case_idx ==:. k, Signal.of_int64 ~width:40 (xmin_bcd_const m)))
    |> Array.to_list
    |> List.fold ~init:(Signal.of_int64 ~width:40 0L) ~f:(fun acc (sel,v) -> mux2 sel v acc)
  in

  let case_xmax (case_idx: Signal.t) : Signal.t =
    Array.init num_cases ~f:(fun k ->
      let m = cases.(k).m in
      (case_idx ==:. k, Signal.of_int64 ~width:40 (xmax_bcd_const m)))
    |> Array.to_list
    |> List.fold ~init:(Signal.of_int64 ~width:40 0L) ~f:(fun acc (sel,v) -> mux2 sel v acc)
  in

  compile
    [
      fifo_lo_wr_en <--. 0;
      fifo_lo_wr_data <--. 0;
      fifo_lo_rd_en <--. 0;

      fifo_hi_wr_en <--. 0;
      fifo_hi_wr_data <--. 0;
      fifo_hi_rd_en <--. 0;

      sm.switch
        [
          (States.Idle,
            [
              when_ uart_rx.valid 
                [
                  if_ (uart_rx.value ==:. 0x02)
                    [
                      is_etx_received <--. 0;

                      num_silly_numbers <--. 0;
                      num_goofy_numbers <--. 0;
                      cur_bcd10 <--. 0;
                      lo_bcd10 <--. 0;

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

                      fifo_lo_wr_en <--. 1;
                      fifo_lo_wr_data <-- lo_bcd10.value;
                      fifo_hi_wr_en <--. 1;
                      fifo_hi_wr_data <-- cur_bcd10.value;

                      cur_bcd10 <--. 0;

                      sm.set_next States.Compute;
                    ][
                      when_ (Math.is_digit uart_rx.value)
                        (
                          let incoming_digit = Math.ascii_to_int8 uart_rx.value in
                          [
                            cur_bcd10 <-- ((select cur_bcd10.value 35 0) @: (uresize incoming_digit 4));
                          ]
                        );

                      when_ (uart_rx.value ==:. Char.to_int '-')
                        [
                          lo_bcd10 <-- cur_bcd10.value;
                          cur_bcd10 <--. 0;
                        ];
                      
                      when_ (uart_rx.value ==:. Char.to_int ',')
                        [
                          fifo_lo_wr_en <--. 1;
                          fifo_lo_wr_data <-- lo_bcd10.value;
                          fifo_hi_wr_en <--. 1;
                          fifo_hi_wr_data <-- cur_bcd10.value;

                          cur_bcd10 <--. 0;
                        ];
                    ]
                ];
            ]);
          (States.Compute,
            [
              when_ (is_etx_received.value &: fifos_empty &: (csm.is Compute_states.Idle))
                [ sm.set_next States.Done; ];
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
              when_ (~:fifo_lo_empty &: ~:fifo_hi_empty)
                [
                  fifo_lo_rd_en <--. 1;
                  fifo_hi_rd_en <--. 1;
                  case_idx <--. 0;
                  
                  csm.set_next Compute_states.Read; 
                ];
            ]);
          (Compute_states.Read,
            [
              range_lo <-- fifo_lo_rd_data;
              range_hi <-- fifo_hi_rd_data;

              csm.set_next Compute_states.Init;
            ]);
          (Compute_states.Init,
            [
              m_r <-- case_m case_idx.value;
              r_r <-- case_r case_idx.value;
              add1_r <-- case_add1 case_idx.value;
              add2_r <-- case_add2 case_idx.value;
              xmin_r <-- case_xmin case_idx.value;
              xmax_r <-- case_xmax case_idx.value;
              pattern <-- case_xmin case_idx.value;

              csm.set_next Compute_states.Evaluate;
            ]);
          (Compute_states.Evaluate,
            let m = m_r.value in
            let r = r_r.value in

            let build_for_r rr =
              let build_for_m =
                let reps_max = 10/rr in
                List.init reps_max ~f:(fun i -> i+1)
                |> List.map ~f:(fun i ->
                  let num_digits = i * rr in
                  let num_pad_digits = 10 - num_digits in
                  let digits = List.init rr ~f:(fun _ -> select pattern.value (i*4-1) 0) |> concat_msb in
                  let digits_padded =
                    if num_pad_digits = 0 then digits
                    else concat_msb [ zero(num_pad_digits*4); digits; ]
                  in
                  (m ==:. i, digits_padded))
                |> List.fold ~init:(zero 40) ~f:(fun acc (sel,v) -> mux2 sel v acc)
              in
              build_for_m
            in

            let cand = 
              List.init 9 ~f:(fun i -> i+2)
              |> List.map ~f:(fun rr -> (r ==:. rr, build_for_r rr))
              |> List.fold ~init:(build_for_r 2) ~f:(fun acc (sel,v) -> mux2 sel v acc)
            in

            let is_cand_in_range = bcd_in_range ~lo:range_lo.value ~hi:range_hi.value ~x:cand in
            let cand_in_bin = bcd_to_bin cand in
            
            let is_pattern_last =
              let compare_pattern_with_xmax mm =
                let w = mm*4 in
                (m ==:. mm, (select pattern.value (w-1) 0) ==: (select xmax_r.value (w-1) 0))
              in

              List.init 10 ~f:(fun i -> i+1)
              |> List.map ~f:compare_pattern_with_xmax
              |> List.fold ~init:gnd ~f:(fun acc (sel,v) -> mux2 sel v acc)
            in

            let pattern_next =
              let increment_pattern mm =
                let w = mm*4 in
                let s = select pattern.value (w-1) 0 in
                let s' = bcd_inc_slice ~digits:mm s in
                let v =
                  if w = 40 then s'
                  else concat_msb [ zero(40-w); s'; ]
                in
                (m ==:. mm, v)
              in

              let init_mm =
                let s = select pattern.value 3 0 in
                let s' = bcd_inc_slice ~digits:1 s in
                concat_msb [ zero(36); s'; ]
              in

              List.init 10 ~f:(fun i -> i+1)
              |> List.map ~f:increment_pattern
              |> List.fold ~init:init_mm ~f:(fun acc (sel,v) -> mux2 sel v acc)
            in

            let are_all_patterns_equal ~(m: int) ~(m': int) (pattern_slice: Signal.t) : Signal.t =
              let ptn_w = m' * 4 in
              let num_ptn = m / m' in
              let ptn0 = select pattern_slice (ptn_w - 1) 0 in
              List.init num_ptn ~f:(fun i -> 
                let lo = i * ptn_w in
                let hi = lo + ptn_w - 1 in
                select pattern_slice hi lo ==: ptn0
                )
              |> List.reduce_exn ~f:(&:)
            in

            let is_pattern_primitive ~(m: int) (pattern_slice: Signal.t) : Signal.t =
              let divisors =
                match m with
                  | 1 -> []
                  | 2 -> [1;]
                  | 3 -> [1;]
                  | 4 -> [1;2;]
                  | 5 -> [1;]
                  | 6 -> [1;2;3;]
                  | 7 -> [1;]
                  | 8 -> [1;2;4;]
                  | 9 -> [1;3;]
                  | 10 -> [1;2;5;]
                  | _ -> []
              in
              let periodic = 
                match divisors with
                  | [] -> gnd
                  | ds -> 
                    ds
                    |> List.map ~f:(fun i -> are_all_patterns_equal ~m ~m':i pattern_slice)
                    |> List.reduce_exn ~f:(|:)
              in
              ~:periodic
            in

            let add_pattern_to_p2_final =
              let is_current_pattern_primitive mm =
                let w = mm * 4 in
                let s = select pattern.value (w-1) 0 in
                (m ==:. mm, is_pattern_primitive ~m:mm s)
              in

              List.init 10 ~f:(fun i -> i+1)
              |> List.map ~f:is_current_pattern_primitive
              |> List.fold ~init:vdd ~f:(fun acc (sel,v) -> mux2 sel v acc)
            in
            [
              when_ is_cand_in_range
                [
                  when_ add1_r.value [ num_silly_numbers <-- (num_silly_numbers.value +: (u64 cand_in_bin)); ];
                ];
              
              when_ (is_cand_in_range &: add_pattern_to_p2_final)
                [
                  when_ add2_r.value [ num_goofy_numbers <-- (num_goofy_numbers.value +: (u64 cand_in_bin)); ];
                ];

              if_ is_pattern_last
                [ csm.set_next Compute_states.Next; ]
                [ pattern <-- pattern_next; ];
            ]);
          (Compute_states.Next,
            [
              if_ (case_idx.value ==:. (num_cases - 1))
                [ csm.set_next Compute_states.Idle; ]
                [
                  case_idx <-- (case_idx.value +:. 1);
                  csm.set_next Compute_states.Init;
                ];
            ]);
        ];
    ];

  (* making the waves *)
  let uart_value = uart_rx.value -- "uart_value" in
  let uart_valid = uart_rx.valid -- "uart_valid" in
  let fifo_lo_wr_en_value = fifo_lo_wr_en.value -- "fifo_lo_wr_en" in
  let fifo_lo_wr_data_value = fifo_lo_wr_data.value -- "fifo_lo_wr_data" in
  let fifo_lo_rd_en_value = fifo_lo_rd_en.value -- "fifo_lo_rd_en" in
  let fifo_lo_rd_data_value = fifo_lo_rd_data -- "fifo_lo_rd_data" in
  let fifo_hi_wr_en_value = fifo_hi_wr_en.value -- "fifo_hi_wr_en" in
  let fifo_hi_wr_data_value = fifo_hi_wr_data.value -- "fifo_hi_wr_data" in
  let fifo_hi_rd_en_value = fifo_hi_rd_en.value -- "fifo_hi_rd_en" in
  let fifo_hi_rd_data_value = fifo_hi_rd_data -- "fifo_hi_rd_data" in
  let is_csm_st_idle = (csm.is Compute_states.Idle) -- "is_csm_st_idle" in
  let is_csm_st_read = (csm.is Compute_states.Read) -- "is_csm_st_read" in
  let is_csm_st_init = (csm.is Compute_states.Init) -- "is_csm_st_init" in
  let is_csm_st_eval = (csm.is Compute_states.Evaluate) -- "is_csm_st_eval" in
  let is_csm_st_next = (csm.is Compute_states.Next) -- "is_csm_st_next" in
  let range_lo_value = range_lo.value -- "range_lo" in
  let range_hi_value = range_hi.value -- "range_hi" in
  let num_cases_value = (Signal.of_int ~width:20 num_cases) -- "num_cases" in
  let case_idx_value = case_idx.value -- "case_idx" in
  let pattern_value = pattern.value -- "pattern" in

  let debug_output =
    concat_msb
      [
        uart_value;
        uart_valid;
        fifos_empty;
        fifos_full;
        fifo_lo_rd_data;
        fifo_hi_rd_data;
        fifo_lo_rd_en_value;
        fifo_lo_wr_en_value;
        fifo_lo_rd_data_value;
        fifo_lo_wr_data_value;
        fifo_hi_rd_en_value;
        fifo_hi_wr_en_value;
        fifo_hi_rd_data_value;
        fifo_hi_wr_data_value;
        is_csm_st_idle;
        is_csm_st_read;
        is_csm_st_init;
        is_csm_st_eval;
        is_csm_st_next;
        range_lo_value;
        range_hi_value;
        case_idx_value;
        num_cases_value;
        pattern_value;
      ]
  in

  num_silly_numbers.value, num_goofy_numbers.value, (sm.is States.Done), debug_output
;;
