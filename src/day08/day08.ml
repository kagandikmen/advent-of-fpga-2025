(*
 *
 * AoF - Hardcaml Solution for Day 8
 * Created:     2025-12-31
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
    | Compute_edges
    | Pad_edges
    | Sort_edges
    | Graph_init
    | Graph_fetch
    | Graph_process
    | Save_p1
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

module Sorter_states = struct
  type t =
    | Idle
    | Read
    | Wait
    | Compare
    | Write_1
    | Write_2
  [@@deriving sexp_of, compare, enumerate]
end


let ( *^: ) a b = uresize (a *: b) (width a)

let abs a b = mux2 (a >=: b) (a -: b) (b -: a)

let sq_u48 d =
  let d48 = uresize d 48 in
  d48 *^: d48

let create_logic ~clock ~clear ~max_vertices (uart_rx: Signal.t Uart.Byte_with_valid.t) =
  let open Always in

  let p1_mark = 50 in
  let root_search_max_depth = 50 in

  let max_edges = (max_vertices * (max_vertices - 1)) / 2 in
  let max_edges_ceil_pow2 = 1 lsl (Math.ceil_log2 max_edges) in

  let vertex_addr_w = Math.ceil_log2 max_vertices in 
  let edge_addr_w = Math.ceil_log2 max_edges_ceil_pow2 in
  let edge_len_w = edge_addr_w + 2 in

  let spec = Reg_spec.create
    ~clock
    ~clear
    ()
  in

  let sm = State_machine.create (module States) spec in 
  let sort_sm = State_machine.create (module Sorter_states) spec in

  let total_p1 = Variable.reg spec ~width:64 in
  let total_p2 = Variable.reg spec ~width:64 in

  let x_coords = Array.init max_vertices ~f:(fun _ -> Variable.reg spec ~width:32) in
  let y_coords = Array.init max_vertices ~f:(fun _ -> Variable.reg spec ~width:32) in
  let z_coords = Array.init max_vertices ~f:(fun _ -> Variable.reg spec ~width:32) in

  let num_vertices = Variable.reg spec ~width:vertex_addr_w in

  let cur_value_receive = Variable.reg spec ~width:32 in
  let coord_sel_receive = Variable.reg spec ~width:2 in
  let tmp_x_receive = Variable.reg spec ~width:32 in
  let tmp_y_receive = Variable.reg spec ~width:32 in
  let _tmp_z_receive = Variable.reg spec ~width:32 in

  let pack_edge ~(dist: Signal.t) ~(u: Signal.t) ~(v: Signal.t) =
    (uresize dist 48) @: (uresize u 8) @: (uresize v 8)
  in

  let unpack_edge_dist e = select e 63 16 in
  let unpack_edge_u e = select e 15 8 in
  let unpack_edge_v e = select e 7 0 in

  let ram_raddr0 = Variable.reg spec ~width:edge_addr_w in
  let ram_raddr1 = Variable.reg spec ~width:edge_addr_w in 
  let ram_waddr = Variable.wire ~default:(zero edge_addr_w) in
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

  let read0_port : Signal.read_port =
    {
      read_clock = clock;
      read_enable = vdd;
      read_address = ram_raddr0.value;
    }
  in

  let read1_port : Signal.read_port =
    {
      read_clock = clock;
      read_enable = vdd;
      read_address = ram_raddr1.value;
    }
  in 

  let ram_rdata = Ram.create
    ~collision_mode:Read_before_write
    ~size:max_edges_ceil_pow2
    ~write_ports:[| write_port |]
    ~read_ports:[| read0_port; read1_port; |]
    ()
  in

  let edge0 = ram_rdata.(0) in
  let edge1 = ram_rdata.(1) in 

  (* for what i and j means here see python solution line 55 *)
  let i_compedges = Variable.reg spec ~width:vertex_addr_w in
  let j_compedges = Variable.reg spec ~width:vertex_addr_w in
  let edge_cnt = Variable.reg spec ~width:edge_len_w in

  let dist_sq ~(u: Signal.t) ~(v: Signal.t) =
    let xu = Procedure.read_array x_coords ~idx:u in
    let yu = Procedure.read_array y_coords ~idx:u in
    let zu = Procedure.read_array z_coords ~idx:u in

    let xv = Procedure.read_array x_coords ~idx:v in
    let yv = Procedure.read_array y_coords ~idx:v in
    let zv = Procedure.read_array z_coords ~idx:v in

    let dx = abs xu xv in
    let dy = abs yu yv in
    let dz = abs zu zv in

    sq_u48 dx +: sq_u48 dy +: sq_u48 dz
  in

  let i_sort = Variable.reg spec ~width:edge_len_w in
  let j_sort = Variable.reg spec ~width:edge_len_w in
  let k_sort = Variable.reg spec ~width:edge_len_w in
  let k_next_sort = Signal.sll k_sort.value 1 in

  let addr0_sorter = Variable.reg spec ~width:edge_len_w in 
  let addr1_sorter = Variable.reg spec ~width:edge_len_w in
  let tmp0_sorter = Variable.reg spec ~width:64 in
  let tmp1_sorter = Variable.reg spec ~width:64 in
  let _do_swap_sorter = Variable.reg spec ~width:1 in
  
  let ascending_sortstate = Variable.reg spec ~width:1 in
  let d0 = (unpack_edge_dist edge0) -- "d0" in
  let d1 = unpack_edge_dist edge1 in
  let do_swap_sortstate = mux2 ascending_sortstate.value (d0 >: d1) (d0 <: d1) in

  let sort_len = Variable.reg spec ~width:edge_len_w in
  let padding_calculated = Variable.reg spec ~width:1 in

  let parents = Array.init max_vertices ~f:(fun _ -> Variable.reg spec ~width:vertex_addr_w) in
  let sizes = Array.init max_vertices ~f:(fun _ -> Variable.reg spec ~width:16) in
  
  let num_circuits = Variable.reg spec ~width:vertex_addr_w in

  let edge_idx = Variable.reg spec ~width:edge_addr_w in
  let num_processed_edges = Variable.reg spec ~width:16 in
  
  let p1_done = Variable.reg spec ~width:1 in
  let p2_done = Variable.reg spec ~width:1 in

  let idx_scan = Variable.reg spec ~width:vertex_addr_w in
  let top_a = Variable.reg spec ~width:16 in
  let top_b = Variable.reg spec ~width:16 in
  let top_c = Variable.reg spec ~width:16 in

  let update_top3 ~(s : Signal.t) =
    [
      if_ (s >: top_a.value)
        [ top_c <-- top_b.value; top_b <-- top_a.value; top_a <-- s ]
        [
          if_ (s >: top_b.value)
            [ top_c <-- top_b.value; top_b <-- s ]
            [
              if_ (s >: top_c.value) [ top_c <-- s ] []
            ]
        ];
    ]
  in

  let find_root (v : Signal.t) =
    let steps = root_search_max_depth in
    let cur = ref v in
    for _k = 1 to steps do
      let p = Procedure.read_array parents ~idx:!cur in
      cur := mux2 (p ==: !cur) !cur p
    done;
    !cur
  in

  let done_ctr = Variable.reg spec ~width:8 in
  let _done_ctr_value = done_ctr.value -- "done_ctr" in
  let done_th = 100 in

  let u8 = (unpack_edge_u edge0) -- "u8" in
  let v8 = (unpack_edge_v edge0) -- "v8" in
  let u = uresize u8 vertex_addr_w in
  let v = uresize v8 vertex_addr_w in
  let ru = (find_root u) -- "ru" in
  let rv = (find_root v) -- "rv" in

  let is_merge_possible = (ru <>: rv) -- "is_merge_possible" in

  let size_ru = Procedure.read_array sizes ~idx:ru in
  let size_rv = Procedure.read_array sizes ~idx:rv in
  
  (* for part 1 only *)
  let xu = Procedure.read_array x_coords ~idx:u in
  let xv = Procedure.read_array x_coords ~idx:v in

  let hit_p1_mark = (p1_done.value ==:. 0) &: (num_processed_edges.value +:. 1 ==:. p1_mark) in

  let parent_savep1 = Procedure.read_array parents ~idx:idx_scan.value in
  let size_savep1 = Procedure.read_array sizes ~idx:idx_scan.value in

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
                  if_ (uart_rx.value ==:. 0x02)
                    [
                      num_vertices <--. 0;
                      i_compedges <--. 0;
                      j_compedges <--. 1;
                      edge_cnt <--. 0;
                      edge_idx <--. 0;
                      sort_len <--. 1;
                      padding_calculated <--. 0;
                      p1_done <--. 0;
                      p2_done <--. 0;
                      total_p1 <--. 0;
                      total_p2 <--. 0;
                      num_processed_edges <--. 0;
                      done_ctr <--. 0;
                      cur_value_receive <--. 0;
                      coord_sel_receive <--. 0;
                      tmp_x_receive <--. 0;
                      tmp_y_receive <--. 0;

                      sm.set_next States.Receive;
                    ][
                      sm.set_next States.Idle;
                    ];
                ];
            ]);
          (States.Receive,
            [
              when_ uart_rx.valid
                [
                  if_ (uart_rx.value ==:. 0x03)
                    [
                      if_ (coord_sel_receive.value ==:. 2)
                        (
                          let idx = num_vertices.value in
                          [
                            proc (Procedure.write_array x_coords ~idx ~data:tmp_x_receive.value);
                            proc (Procedure.write_array y_coords ~idx ~data:tmp_y_receive.value);
                            proc (Procedure.write_array z_coords ~idx ~data:cur_value_receive.value);
                            num_vertices <-- (num_vertices.value +:. 1);
                            tmp_x_receive <--. 0;
                            tmp_y_receive <--. 0;
                            cur_value_receive <--. 0;
                            coord_sel_receive <--. 0;
                          ]
                        )[
                        ];
                      
                      sm.set_next States.Compute_edges;
                    ][
                      if_ (Math.is_digit uart_rx.value)
                        [
                          cur_value_receive <-- ((Math.mul10 cur_value_receive.value) +: (uresize (uart_rx.value -:. Char.to_int '0') 32));
                        ][
                          if_ (uart_rx.value ==:. Char.to_int ',')
                            [
                              if_ (coord_sel_receive.value ==:. 0) [ tmp_x_receive <-- cur_value_receive.value; ][];
                              if_ (coord_sel_receive.value ==:. 1) [ tmp_y_receive <-- cur_value_receive.value; ][];
                              cur_value_receive <--. 0;
                              coord_sel_receive <-- (coord_sel_receive.value +:. 1);
                            ][
                              if_ (uart_rx.value ==:. Char.to_int '\n')
                                [
                                  proc (Procedure.write_array x_coords ~idx:num_vertices.value ~data:tmp_x_receive.value);
                                  proc (Procedure.write_array y_coords ~idx:num_vertices.value ~data:tmp_y_receive.value);
                                  proc (Procedure.write_array z_coords ~idx:num_vertices.value ~data:cur_value_receive.value);
                                  num_vertices <-- (num_vertices.value +:. 1);
                                  tmp_x_receive <--. 0;
                                  tmp_y_receive <--. 0;
                                  cur_value_receive <--. 0;
                                  coord_sel_receive <--. 0;
                                ][
                                  (* does not happen *)
                                ]
                            ]
                        ]
                    ];
                ];
            ]);
          (States.Compute_edges,
            [
              if_ (i_compedges.value +:. 1 >=: num_vertices.value)
                [
                  padding_calculated <--. 1;
                  sort_len <--. 1;
                  sm.set_next States.Pad_edges;
                ](
                  let dist = dist_sq ~u:i_compedges.value ~v:j_compedges.value in
                  [
                    ram_we <--. 1;
                    ram_waddr <-- uresize edge_cnt.value edge_addr_w;
                    ram_wdata <-- (pack_edge ~dist ~u:(uresize i_compedges.value 8) ~v:(uresize j_compedges.value 8));
                    edge_cnt <-- (edge_cnt.value +:. 1);

                    if_ (j_compedges.value +:. 1 >=: num_vertices.value)
                      [
                        i_compedges <-- (i_compedges.value +:. 1);
                        j_compedges <-- (i_compedges.value +:. 2);
                      ][
                        j_compedges <-- (j_compedges.value +:. 1);
                      ]
                  ]
                )
            ]);
          (States.Pad_edges,
            [
              when_ (padding_calculated.value ==:. 1)
                [
                  if_ (sort_len.value >=: edge_cnt.value)
                    [ padding_calculated <--. 0; ]
                    [ sort_len <-- (Signal.sll sort_len.value 1); ]
                ];

              when_ (padding_calculated.value ==:. 0)
                [
                  if_ (edge_cnt.value >=: sort_len.value)
                    [
                      i_sort <--. 0;
                      j_sort <--. 1;
                      k_sort <--. 2;
                      sm.set_next States.Sort_edges;
                    ][
                      ram_we <--. 1;
                      ram_waddr <-- uresize edge_cnt.value edge_addr_w;
                      ram_wdata <-- pack_edge ~dist:(ones 48) ~u:(zero 8) ~v:(zero 8);
                      edge_cnt <-- (edge_cnt.value +:. 1);
                    ]
                ];
            ]);
          (States.Sort_edges,
            [
              if_ (k_sort.value >: sort_len.value)
                [
                  idx_scan <--. 0;
                  sm.set_next States.Graph_init;
                ][
                  when_ (sort_sm.is Sorter_states.Idle)
                    [
                      when_ ((i_sort.value ^: j_sort.value) >: i_sort.value)
                        [
                          addr0_sorter <-- i_sort.value;
                          addr1_sorter <-- (i_sort.value ^: j_sort.value);
                          ascending_sortstate <-- ((i_sort.value &: k_sort.value) ==:. 0);
                          sort_sm.set_next Sorter_states.Read;
                        ];

                      if_ (i_sort.value +:. 1 >=: sort_len.value)
                        [
                          i_sort <--. 0;
                          if_ (j_sort.value ==:. 1)
                            [
                              k_sort <-- k_next_sort;
                              j_sort <-- (Signal.srl k_next_sort 1);
                            ][
                              j_sort <-- (Signal.srl j_sort.value 1);
                            ]
                        ][
                          i_sort <-- (i_sort.value +:. 1);
                        ]
                    ]
                ]
            ]);
          (States.Graph_init,
            [
              when_ (idx_scan.value ==:. 0)
                [
                  num_circuits <-- num_vertices.value;
                ];

              if_ (idx_scan.value >=: num_vertices.value)
                [
                  sm.set_next States.Graph_fetch;
                ][
                  proc (Procedure.write_array parents ~idx:idx_scan.value ~data:idx_scan.value);
                  proc (Procedure.write_array sizes ~idx:idx_scan.value ~data:(one 16));
                  idx_scan <-- (idx_scan.value +:. 1);
                ]
            ]);
          (States.Graph_fetch,
            [
              if_ (p1_done.value &: p2_done.value)
                [ sm.set_next States.Done; ]
                [
                  ram_raddr0 <-- edge_idx.value;
                  sm.set_next States.Graph_process;
                ]
            ]);
          (States.Graph_process,
            [
              sm.set_next States.Graph_fetch;

              num_processed_edges <-- (num_processed_edges.value +:. 1);
              edge_idx <-- (edge_idx.value +:. 1);

              when_ is_merge_possible
                (
                  let u_gt_v = size_ru >=: size_rv in
                  let r_greater = mux2 u_gt_v ru rv in
                  let r_smaller = mux2 u_gt_v rv ru in 
                  let s_greater = mux2 u_gt_v size_ru size_rv in
                  let s_smaller = mux2 u_gt_v size_rv size_ru in
                  [
                    proc (Procedure.write_array parents ~idx:r_smaller ~data:r_greater);
                    proc (Procedure.write_array sizes ~idx:r_greater ~data:(s_smaller +: s_greater));
                    num_circuits <-- (num_circuits.value -:. 1);

                    when_ (num_circuits.value ==:. 2)
                      [
                        total_p2 <-- uresize (xu *^: xv) 64;
                        p2_done <--. 1;
                      ];
                  ]
                );

              when_ hit_p1_mark
                [
                  p1_done <--. 1;
                  idx_scan <--. 0;
                  top_a <--. 0;
                  top_b <--. 0;
                  top_c <--. 0;
                  sm.set_next States.Save_p1;
                ];
            ]);
          (States.Save_p1,
            let top_a_64 = uresize top_a.value 64 in
            let top_b_64 = uresize top_b.value 64 in
            let top_c_64 = uresize top_c.value 64 in
            [
              if_ (idx_scan.value >=: num_vertices.value)
                [
                  total_p1 <-- ((top_a_64 *^: top_b_64) *^: top_c_64);
                  sm.set_next States.Graph_fetch;
                ][
                  when_ (parent_savep1 ==: idx_scan.value)
                    [
                      proc (update_top3 ~s:size_savep1);
                    ];
                  idx_scan <-- (idx_scan.value +:. 1);
                ];
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

      sort_sm.switch
        [
          (Sorter_states.Idle,
            [
            ]);
          (Sorter_states.Read,
            [
              ram_raddr0 <-- uresize addr0_sorter.value edge_addr_w;
              ram_raddr1 <-- uresize addr1_sorter.value edge_addr_w;
              sort_sm.set_next Sorter_states.Wait;
            ]);
          (Sorter_states.Wait,
            [
              sort_sm.set_next Sorter_states.Compare;
            ]);
          (Sorter_states.Compare,
            [
              tmp0_sorter <-- edge0;
              tmp1_sorter <-- edge1;
              if_ do_swap_sortstate
                [ sort_sm.set_next Sorter_states.Write_1; ]
                [ sort_sm.set_next Sorter_states.Idle; ]
            ]);
          (Sorter_states.Write_1,
            [
              ram_we <--. 1;
              ram_waddr <-- uresize ram_raddr0.value edge_addr_w;
              ram_wdata <-- tmp1_sorter.value;
              sort_sm.set_next Sorter_states.Write_2;
            ]);
          (Sorter_states.Write_2,
            [
              ram_we <--. 1;
              ram_waddr <-- uresize ram_raddr1.value edge_addr_w;
              ram_wdata <-- tmp0_sorter.value;
              sort_sm.set_next Sorter_states.Idle;
            ]);
        ];
    ];


  (* some waveform debugging *)
  let _uart_value = uart_rx.value -- "uart_value" in
  let _uart_valid = uart_rx.valid -- "uart_valid" in
  let is_sm_idle = sm.is States.Idle -- "is_sm_idle" in
  let is_sm_receive = sm.is States.Receive -- "is_sm_receive" in
  let is_sm_compute_edges = sm.is States.Compute_edges -- "is_sm_compedges" in
  let is_sm_pad_edges = sm.is States.Pad_edges -- "is_sm_pad_edges" in
  let is_sm_sort = sm.is States.Sort_edges -- "is_sm_sort" in
  let is_sm_graph_init = sm.is States.Graph_init -- "is_sm_graph_init" in
  let is_sm_graph_fetch = sm.is States.Graph_fetch -- "is_sm_graph_fetch" in
  let is_sm_graph_process = sm.is States.Graph_process -- "is_sm_graph_process" in
  let is_sm_save_p1 = sm.is States.Save_p1 -- "is_sm_save_p1" in
  let is_sm_done = sm.is States.Done -- "is_sm_done" in
  let num_vertices_value = num_vertices.value -- "num_vertices" in
  let edge0_value = edge0 -- "edge0" in
  let edge1_value = edge1 -- "edge1" in
  let i_sort_value = i_sort.value -- "i_sort" in
  let j_sort_value = j_sort.value -- "j_sort" in
  let k_sort_value = k_sort.value -- "k_sort" in
  let padding_calculated_value = padding_calculated.value -- "padding_calculated" in
  let num_circuits_value = num_circuits.value -- "num_circuits" in
  let idx_scan_value = idx_scan.value -- "idx_scan" in
  let edge_idx_value = edge_idx.value -- "edge_idx" in
  let edge_cnt_value = edge_cnt.value -- "edge_cnt" in
  let sort_len_value = sort_len.value -- "sort_len" in
  let merge_triggered = (is_merge_possible &: (sm.is States.Graph_process)) -- "merge_triggered" in
  let num_processed_edges_value = num_processed_edges.value -- "num_processed_edges" in
  let p1_done_value = p1_done.value -- "p1_done" in
  let p2_done_value = p2_done.value -- "p2_done" in

  let debug_output =
    is_sm_idle
    |: is_sm_receive
    |: is_sm_compute_edges
    |: is_sm_pad_edges
    |: is_sm_sort
    |: is_sm_graph_init
    |: is_sm_graph_fetch
    |: is_sm_graph_process
    |: is_sm_save_p1
    |: is_sm_done
    |: (num_vertices_value >:. 0)
    |: (edge0_value >:. 0)
    |: (edge1_value >:. 0)
    |: (i_sort_value >:. 0)
    |: (j_sort_value >:. 0)
    |: (k_sort_value >:. 0)
    |: padding_calculated_value
    |: (num_circuits_value >:. 0)
    |: (idx_scan_value >:. 0)
    |: (edge_idx_value >:. 0)
    |: (edge_cnt_value >:. 0)
    |: (sort_len_value >:. 0)
    |: merge_triggered
    |: (num_processed_edges_value >:. 0)
    |: p1_done_value
    |: p2_done_value
  in

  total_p1.value, total_p2.value, (sm.is States.Done), debug_output
;;

let create ~clock ~clear ~cycles_per_bit ~max_vertices uart_rx_value =
  let uart_rx = Uart.Expert.create_rx_state_machine
    ~clock
    ~clear
    ~cycles_per_bit
    uart_rx_value
  in
  create_logic ~clock ~clear ~max_vertices uart_rx
;;