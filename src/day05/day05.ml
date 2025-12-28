(*
 *
 * AoF - Hardcaml Solution for Day 5 (Step 1 & Step 2)
 * Created:     2025-12-24
 * Modified:    2025-12-26
 * Author:      Kagan Dikmen
 *
 *)

(* TODO: Add FPGA build and RTL generation logic *)

open! Core
open! Hardcaml
open! Hardcaml_aof
open! Hardcaml_arty
open! Signal

module Package = struct
  type 'a t =
    {
      flag: 'a;
      payload: 'a;
    }
end

module Package_states = struct
  type t =
    | Read_flag   (* this also serves as the "wait" state *)
    | Read_payload
  [@@deriving sexp_of, compare, enumerate]
end

module States = struct
  type t =
    | Read_ranges
    | Read_ids
    | Scan
    | Merge
    | Count
    | Done
  [@@deriving sexp_of, compare, enumerate]
end


(* main logic*)

let create_day05_logic ~clock ~clear ~cycles_per_bit uart_rx_value =
  let open Always in

  let max_range_count = 200 in
  let max_mrange_count = 200 in
  let max_id_count = 1024 in

  let uart_rx = Uart.Expert.create_rx_state_machine
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in

  (* for debugging *)
  let _uart_valid = uart_rx.valid -- "uart_valid" in
  let _uart_value = uart_rx.value -- "uart_value" in

  let spec = Reg_spec.create
    ~clock
    ~clear
    ()
  in

  let num_fresh = Variable.reg spec ~width:64 in
  let num_covered = Variable.reg spec ~width:64 in

  let package_sm = State_machine.create (module Package_states) spec in
  let sm = State_machine.create (module States) spec in

  let pkg_flag = Variable.reg spec ~width:8 in
  let pkg_payload = Variable.reg spec ~width:64 in
  let pkg_payload_idx = Variable.reg spec ~width:3 in
  let pkg_valid = Variable.reg spec ~width:1 in

  (* i know this looks hideous: *)
  let sll_incoming_byte (idx: Signal.t) (b: Signal.t) : Signal.t =
    let b64 = uresize b 64 in
    mux2 (idx ==:. 0)
      (sll b64 0)
      (mux2 (idx ==:. 1)
        (sll b64 8)
        (mux2 (idx ==:. 2)
          (sll b64 16)
          (mux2 (idx ==:. 3)
            (sll b64 24)
            (mux2 (idx ==:. 4)
              (sll b64 32)
              (mux2 (idx ==:. 5)
                (sll b64 40)
                (mux2 (idx ==:. 6)
                  (sll b64 48)
                  (mux2 (idx ==:. 7)
                    (sll b64 56)
                    (zero 64))))))))
  in

  (* package fsm *)
  compile
    [
      package_sm.switch
        [
          (Package_states.Read_flag, 
            [
              pkg_valid <--. 0;
              when_ uart_rx.valid
                [
                  pkg_flag <-- uart_rx.value;
                  pkg_payload <--. 0;
                  pkg_payload_idx <--. 0;
                  package_sm.set_next Package_states.Read_payload;
                ]
            ]);
          (Package_states.Read_payload,
            [
              pkg_valid <--. 0;
              when_ uart_rx.valid
                [
                  pkg_payload <-- (pkg_payload.value |: (sll_incoming_byte pkg_payload_idx.value uart_rx.value));
                  pkg_payload_idx <-- (pkg_payload_idx.value +:. 1);
                  when_ (pkg_payload_idx.value ==:. 7)
                    [
                      pkg_valid <--. 1;
                      package_sm.set_next Package_states.Read_flag;
                    ]
                ]
            ]);
        ]
    ];

  let pkg: Signal.t Package.t =
    {
      flag = pkg_flag.value;
      payload = pkg_payload.value;
    }
  in

  let _pkg_flag = pkg.flag -- "pkg_flag" in
  let _pkg_payload = pkg.payload -- "pky_payload" in

  let flag_lo = pkg.flag ==:. 0x01 in
  let flag_hi = pkg.flag ==:. 0x02 in
  let flag_sc = pkg.flag ==:. 0x03 in
  let flag_id = pkg.flag ==:. 0x04 in
  let flag_eof = pkg.flag ==:. 0xFF in

  let range_count = Variable.reg spec ~width:(Math.ceil_log2 max_range_count) in
  let ranges_lo = Array.init max_range_count ~f:(fun _ -> Variable.reg spec ~width:64) in
  let ranges_hi = Array.init max_range_count ~f:(fun _ -> Variable.reg spec ~width:64) in
  let ranges_used = Array.init max_range_count ~f:(fun _ -> Variable.reg spec ~width:1) in

  (* "i already read the lower bound but not the upper bound yet" registers: *)
  let lo_waiting = Variable.reg spec ~width:64 in
  let have_lo_waiting = Variable.reg spec ~width:1 in

  let id_count = Variable.reg spec ~width:(Math.ceil_log2 max_id_count) in
  let id_i = Variable.reg spec ~width:(Math.ceil_log2 max_id_count) in
  let ids = Array.init max_id_count ~f:(fun _ -> Variable.reg spec ~width:64) in

  let mrange_count = Variable.reg spec ~width:(Math.ceil_log2 max_mrange_count) in
  let mranges_lo = Array.init max_mrange_count ~f:(fun _ -> Variable.reg spec ~width:64) in
  let mranges_hi = Array.init max_mrange_count ~f:(fun _ -> Variable.reg spec ~width:64) in

  (* current range = the range i am scanning a higher upper bound for right now *)
  let cur_lo = Variable.reg spec ~width:64 in
  let cur_hi = Variable.reg spec ~width:64 in
  let have_cur = Variable.reg spec ~width:1 in

  let push_current_range = 
    [
      when_ (have_cur.value ==:. 1)
        (
          [
            num_covered <-- (num_covered.value +: ((cur_hi.value -: cur_lo.value) +:. 1));
          ]
          @ Procedure.write_array mranges_lo ~idx:mrange_count.value ~data:cur_lo.value
          @ Procedure.write_array mranges_hi ~idx:mrange_count.value ~data:cur_hi.value
          @ [
            mrange_count <-- (mrange_count.value +:. 1);
            have_cur <--. 0
          ]
        )
    ]
  in

  let is_id_fresh (id: Signal.t) =
    let hits = List.init max_mrange_count ~f:(fun i ->
      let mrange_exists = mrange_count.value >:. i in
      let id_in_range = (id >=: mranges_lo.(i).value) &: (id <=: mranges_hi.(i).value) in
      mrange_exists &: id_in_range)
    in
    List.reduce_exn hits ~f:(|:)
  in

  let scan_i = Variable.reg spec ~width:(width range_count.value) in
  let scan_best_i = Variable.reg spec ~width:(width range_count.value) in
  let scan_best_lo = Variable.reg spec ~width:64 in
  let scan_best_hi = Variable.reg spec ~width:64 in
  let scan_found_any = Variable.reg spec ~width:1 in

  let set_scan_up =
    [
      scan_i <--. 0;
      scan_found_any <--. 0;
      scan_best_lo <-- ones 64;
      scan_best_hi <--. 0;
      scan_best_i <--. 0;
    ]
  in

  let scan_count_up =
    [
      num_fresh <--. 0;
      id_i <--. 0;
    ]
  in
    
  compile
    [
      sm.switch
        [
          (States.Read_ranges,
            [
              when_ (pkg_valid.value &: flag_lo)
                [
                  lo_waiting <-- pkg.payload;
                  have_lo_waiting <-- vdd;
                ];
              
              when_ (pkg_valid.value &: flag_hi &: have_lo_waiting.value)
                (
                  Procedure.write_array ranges_lo ~idx:range_count.value ~data:lo_waiting.value
                  @ Procedure.write_array ranges_hi ~idx:range_count.value ~data:pkg.payload
                  @ Procedure.write_array ranges_used ~idx:range_count.value ~data:gnd
                  @ [
                    range_count <-- (range_count.value +:. 1);
                  ]
                );
              
              when_ (pkg_valid.value &: flag_sc)
                [
                  sm.set_next States.Read_ids
                ];
            ]);
          (States.Read_ids,
            [
              when_ (pkg_valid.value &: flag_id)
                (
                  Procedure.write_array ids ~idx:id_count.value ~data:pkg.payload
                  @ [
                    id_count <-- (id_count.value +:. 1);
                  ]
                );

              when_ (pkg_valid.value &: flag_eof)
                (
                  set_scan_up
                  @ [
                    sm.set_next States.Scan;
                  ]
                );
            ]);
          (States.Scan,
            [
              when_ (scan_i.value <>: range_count.value)
                (
                  let i = scan_i.value in
                  let used_i = Procedure.read_array ranges_used ~idx:i in
                  let lo_i = Procedure.read_array ranges_lo ~idx:i in
                  let hi_i = Procedure.read_array ranges_hi ~idx:i in
                  [
                    when_ (used_i ==:. 0)
                      (
                        let is_better = (scan_found_any.value ==:. 0) |: (lo_i <: scan_best_lo.value) in
                        [
                          when_ is_better
                            [
                              scan_best_i <-- i; 
                              scan_best_lo <-- lo_i;
                              scan_best_hi <-- hi_i;
                              scan_found_any <--. 1;
                            ];
                        ]
                      );
                    scan_i <-- (scan_i.value +:. 1);
                  ];
                );

              when_ (scan_i.value ==: range_count.value)
                [
                  sm.set_next States.Merge;
                ];
            ]);
          (States.Merge,
            [
              when_ (scan_found_any.value ==:. 0)
                (
                  push_current_range
                  @ scan_count_up
                  @ [
                    sm.set_next States.Count
                  ]
                );
              
              when_ (scan_found_any.value ==:. 1)
                (
                  let hi = scan_best_hi.value in
                  let lo = scan_best_lo.value in
                  (
                    Procedure.write_array ranges_used ~idx:scan_best_i.value ~data:vdd
                    @ [
                      when_ (have_cur.value ==:. 0)
                        [
                          cur_lo <-- lo;
                          cur_hi <-- hi;
                          have_cur <--. 1;
                        ];
                      when_ (have_cur.value ==:. 1)
                        (
                          let overlap = lo <=: (cur_hi.value +:. 1) in
                          [
                            when_ overlap
                              [
                                cur_hi <-- mux2 (hi >: cur_hi.value) hi cur_hi.value;
                              ];
                            when_ (~:overlap)
                              (
                                push_current_range
                                @ [
                                  cur_lo <-- lo;
                                  cur_hi <-- hi;
                                  have_cur <--. 1;
                                ]
                              );
                          ]
                        )
                    ]
                    @ set_scan_up
                    @ [
                      sm.set_next States.Scan;
                    ]
                  )
                );
            ]);
          (States.Count,
            [
              when_ (id_i.value ==: id_count.value)
                [
                  sm.set_next States.Done
                ];
              
              when_ (id_i.value <>: id_count.value)
                (
                  let is_fresh = Procedure.read_array ids ~idx:id_i.value |> is_id_fresh in
                  [
                    when_ is_fresh
                      [
                        num_fresh <-- (num_fresh.value +:. 1);
                      ];
                    id_i <-- (id_i.value +:. 1);
                  ]
                );
            ]);
          (States.Done,
            [
              (*  this is your signal to take a 5-minute break: https://www.youtube.com/watch?v=q3uXXh1sHcI  *)
            ]);
        ]
    ];
  
  num_fresh.value, num_covered.value, (sm.is States.Done)
;;
