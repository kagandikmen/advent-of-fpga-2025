(*
 *
 * AoF - Hardcaml Solution for Day 11 (Step 1 & Step 2)
 * Created:     2026-01-03
 * Modified:    2026-01-12
 * Author:      Kagan Dikmen
 *
 *)

(* TODO: Add FPGA build and RTL generation logic *)

(*
 * n: node
 * e: edge
 * nn: node name
 * nid: node id
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
    | Find_node
    | Add_node
    | Init_top
    | Pop_top
    | Dive_top
    | Clear_memo
    | Switch_goal
    | Switch_node
    | Dive
    | Save_to_memo
    | Conclude_pass
    | Conclude
    | Done
  [@@deriving sexp_of, compare, enumerate]
end

let ( *^: ) a b = uresize (a *: b) (width a)

let read_array = Procedure.read_array
let write_array = Procedure.write_array

let create ~clock ~clear ~cycles_per_bit uart_rx_value =
  let open Always in

  let max_nodes = 512 in
  let max_edges = 2048 in

  let node_addr_w = Math.ceil_log2 max_nodes in
  let edge_addr_w = Math.ceil_log2 max_edges in

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

  let node_nn = Array.init max_nodes ~f:(fun _ -> Variable.reg spec ~width:32) in

  let nid_you = Variable.reg spec ~width:node_addr_w in 
  let nid_out = Variable.reg spec ~width:node_addr_w in
  let nid_svr = Variable.reg spec ~width:node_addr_w in
  let nid_dac = Variable.reg spec ~width:node_addr_w in
  let nid_fft = Variable.reg spec ~width:node_addr_w in
  let nid_goal = Variable.reg spec ~width:node_addr_w in

  let n_first_edge = Array.init max_nodes ~f:(fun _ -> Variable.reg spec ~width:edge_addr_w) in
  let e_head_node = Array.init max_edges ~f:(fun _ -> Variable.reg spec ~width:node_addr_w) in
  let e_next = Array.init max_edges ~f:(fun _ -> Variable.reg spec ~width:edge_addr_w) in

  let n_indeg = Array.init max_nodes ~f:(fun _ -> Variable.reg spec ~width:16) in
  let n_indeg_copy = Array.init max_nodes ~f:(fun _ -> Variable.reg spec ~width:16) in

  let top_order = Array.init max_nodes ~f:(fun _ -> Variable.reg spec ~width:node_addr_w) in

  let n_ways_to_goal = Array.init max_nodes ~f:(fun _ -> Variable.reg spec ~width:64) in

  let num_nodes = Variable.reg spec ~width:node_addr_w in
  let num_edges = Variable.reg spec ~width:edge_addr_w in

  let kq = Array.init max_nodes ~f:(fun _ -> Variable.reg spec ~width:node_addr_w) in
  let kq_head = Variable.reg spec ~width:node_addr_w in
  let kq_tail = Variable.reg spec ~width:node_addr_w in
  let kq_count = Variable.reg spec ~width:(node_addr_w + 1) in

  let kq_push (x: Signal.t) =
    [
      proc (write_array kq ~idx:kq_tail.value ~data:x);
      kq_tail <-- (kq_tail.value +:. 1);
      kq_count <-- (kq_count.value +:. 1);
    ]
  in

  let cur_nn = Variable.reg spec ~width:32 in
  let nn_receive_ongoing = Variable.reg spec ~width:1 in
  let nid_src = Variable.reg spec ~width:node_addr_w in
  let is_node_target = Variable.reg spec ~width:1 in

  let is_etx_received = Variable.reg spec ~width:1 in

  let find_idx = Variable.reg spec ~width:node_addr_w in
  let found_id = Variable.reg spec ~width:node_addr_w in
  let found = Variable.reg spec ~width:1 in
  let is_nn_src = Variable.reg spec ~width:1 in

  let inittop_idx = Variable.reg spec ~width:node_addr_w in 
  let poptop_idx = Variable.reg spec ~width:node_addr_w in 
  let divetop_idx = Variable.reg spec ~width:edge_addr_w in

  let pass_idx = Variable.reg spec ~width:2 in

  let clearmemo_idx = Variable.reg spec ~width:node_addr_w in
  let savetomemo_idx = Variable.reg spec ~width:node_addr_w in
  let dive_idx = Variable.reg spec ~width:edge_addr_w in
  let dive_sum = Variable.reg spec ~width:64 in

  let you_out = Variable.reg spec ~width:64 in
  let dac_out = Variable.reg spec ~width:64 in
  let fft_out = Variable.reg spec ~width:64 in
  let svr_dac = Variable.reg spec ~width:64 in
  let svr_fft = Variable.reg spec ~width:64 in
  let fft_dac = Variable.reg spec ~width:64 in
  let dac_fft = Variable.reg spec ~width:64 in

  let is_stx = uart_rx.value ==:. 0x02 in
  let is_etx = uart_rx.value ==:. 0x03 in
  let is_space = uart_rx.value ==:. Char.to_int ' ' in
  let is_nl = uart_rx.value ==:. Char.to_int '\n' in
  let is_colon = uart_rx.value ==:. Char.to_int ':' in

  let nil_edge = ones edge_addr_w in

  let pack_nn (s: string) =
    String.to_list s
    |> List.fold ~init:0 ~f:(fun acc ch -> (acc lsl 8) lor Char.to_int ch)
  in

  let nn_you = pack_nn "you" in
  let nn_out = pack_nn "out" in
  let nn_svr = pack_nn "svr" in
  let nn_dac = pack_nn "dac" in
  let nn_fft = pack_nn "fft" in

  let t_lookup = read_array node_nn ~idx:find_idx.value in
  let hit_lookup = t_lookup ==: cur_nn.value in

  let insert_cond = (found.value ==:. 0) &: (find_idx.value ==: num_nodes.value) in
  let nid_cur = mux2 insert_cond num_nodes.value found_id.value in

  let goal_sel =
    mux pass_idx.value
      [
        nid_out.value;
        nid_dac.value;
        nid_fft.value;
        (zero node_addr_w); (* pass_idx = 3 is invalid *)
      ]
  in

  let cur_node = read_array top_order ~idx:savetomemo_idx.value in
  let cur_first_edge = read_array n_first_edge ~idx:cur_node in

  let add_edge ~(u: Signal.t) ~(v: Signal.t) =
    [
      proc (write_array e_head_node ~idx:num_edges.value ~data:v);
      proc (write_array e_next ~idx:num_edges.value ~data:(read_array n_first_edge ~idx:u));
      proc (write_array n_first_edge ~idx:u ~data:num_edges.value);
      proc (write_array n_indeg ~idx:v ~data:((read_array n_indeg ~idx:v) +:. 1));
      num_edges <-- (num_edges.value +:. 1);
    ]
  in

  let prepare_find_node ~(is_src: Signal.t) =
    [
      is_nn_src <-- is_src;
      find_idx <--. 0;
      found <--. 0;
      found_id <--. 0;
    ]
  in

  let reset_graph =
    [
      num_nodes <--. 0;
      num_edges <--. 0;
    ]
  in

  let done_ctr = Variable.reg spec ~width:8 in
  let done_th = 100 in

  compile
    [
      p1_result <-- p1_result.value;
      p2_result <-- p2_result.value;

      sm.switch
        [
          (States.Idle,
            [
              when_ uart_rx.valid
                [
                  if_ is_stx
                    [
                      proc reset_graph;

                      cur_nn <--. 0;
                      nn_receive_ongoing <--. 0;
                      is_node_target <--. 0;
                      nid_src <--. 0;
                      is_etx_received <--. 0;

                      nid_you <--. 0;
                      nid_out <--. 0;
                      nid_svr <--. 0;
                      nid_dac <--. 0;
                      nid_fft <--. 0;

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
                  if_ is_etx
                    [
                      if_ (nn_receive_ongoing.value ==:. 1)
                        [
                          is_etx_received <--. 1;
                          proc (prepare_find_node ~is_src:(~:(is_node_target.value)));
                          sm.set_next States.Find_node;
                        ][
                          is_etx_received <--. 0;
                          inittop_idx <--. 0;
                          poptop_idx <--. 0;
                          kq_head <--. 0;
                          kq_tail <--. 0;
                          kq_count <--. 0;
                          sm.set_next States.Init_top;
                        ]
                    ][
                      if_ (is_space |: is_nl |: is_colon)
                        [
                          when_ is_colon [ is_node_target <--. 1; ];
                          when_ is_nl [ is_node_target <--. 0; ];

                          proc (prepare_find_node ~is_src:(~:(is_node_target.value)));
                          sm.set_next States.Find_node;
                        ][
                          cur_nn <-- uresize (cur_nn.value @: uart_rx.value) 32;
                          nn_receive_ongoing <--. 1;
                        ]
                    ]
                ];
            ]);
          (States.Find_node,
            [
              if_ (find_idx.value ==: num_nodes.value)
                [ sm.set_next States.Add_node; ]
                [
                  if_ hit_lookup
                    [
                      found <--. 1;
                      found_id <-- find_idx.value;
                      sm.set_next States.Add_node;
                    ][
                      find_idx <-- (find_idx.value +:. 1);
                    ];
                ]
            ]);
          (States.Add_node,
            [
              when_ insert_cond
                [
                  proc (write_array node_nn ~idx:num_nodes.value ~data:cur_nn.value);
                  proc (write_array n_first_edge ~idx:num_nodes.value ~data:nil_edge);
                  proc (write_array n_indeg ~idx:num_nodes.value ~data:(zero 16));
                  found_id <-- num_nodes.value;
                  num_nodes <-- (num_nodes.value +:. 1);
                ];

              when_ (cur_nn.value ==:. nn_you) [ nid_you <-- nid_cur; ];
              when_ (cur_nn.value ==:. nn_out) [ nid_out <-- nid_cur; ];
              when_ (cur_nn.value ==:. nn_svr) [ nid_svr <-- nid_cur; ];
              when_ (cur_nn.value ==:. nn_dac) [ nid_dac <-- nid_cur; ];
              when_ (cur_nn.value ==:. nn_fft) [ nid_fft <-- nid_cur; ];

              if_ (is_nn_src.value ==:. 1)
                [
                  nid_src <-- nid_cur;
                  is_node_target <--. 1;
                ][
                  proc (add_edge ~u:nid_src.value ~v:nid_cur);
                ];

              cur_nn <--. 0;
              nn_receive_ongoing <--. 0;

              if_ (is_etx_received.value ==:. 1)
                [
                  is_etx_received <--. 0;
                  inittop_idx <--. 0;
                  poptop_idx <--. 0;
                  kq_head <--. 0;
                  kq_tail <--. 0;
                  kq_count <--. 0;
                  sm.set_next States.Init_top;
                ][
                  sm.set_next States.Receive;
                ];
            ]);
          (States.Init_top,
            let n_indeg_cur = read_array n_indeg ~idx:inittop_idx.value in
            [
              if_ (inittop_idx.value ==: num_nodes.value)
                [
                  poptop_idx <--. 0;
                  sm.set_next States.Pop_top;
                ][
                  proc (write_array n_indeg_copy ~idx:inittop_idx.value ~data:n_indeg_cur); (* this is just copying n_indeg -> n_indeg_copy *)
                  when_ (n_indeg_cur ==:. 0) (kq_push inittop_idx.value); (* push to kahn queue *)
                  inittop_idx <-- (inittop_idx.value +:. 1);
                ];
            ]);
          (States.Pop_top,
            let kq_head_node = read_array kq ~idx:kq_head.value in
            let kq_head_node_first_edge = read_array n_first_edge ~idx:kq_head_node in
            [
              if_ (kq_count.value ==:. 0)
                [
                  pass_idx <--. 0;
                  clearmemo_idx <--. 0;
                  sm.set_next States.Clear_memo;
                ][
                  kq_head <-- (kq_head.value +:. 1);
                  kq_count <-- (kq_count.value -:. 1);

                  proc (write_array top_order ~idx:poptop_idx.value ~data:kq_head_node);
                  poptop_idx <-- (poptop_idx.value +:. 1);

                  divetop_idx <-- kq_head_node_first_edge;
                  sm.set_next States.Dive_top;
                ]
            ]);
          (States.Dive_top,
            let topo_v_sig = read_array e_head_node ~idx:divetop_idx.value in
            let n_indeg_v = read_array n_indeg_copy ~idx:topo_v_sig in
            let n_indeg_v' = n_indeg_v -:. 1 in
            [
              if_ (divetop_idx.value ==: nil_edge)
                [
                  sm.set_next States.Pop_top;
                ][
                  proc (write_array n_indeg_copy ~idx:topo_v_sig ~data:n_indeg_v');
                  when_ (n_indeg_v' ==:. 0) (kq_push topo_v_sig);
                  divetop_idx <-- (read_array e_next ~idx:divetop_idx.value);
                ];
            ]);
          (States.Clear_memo,
            [
              if_ (clearmemo_idx.value ==: num_nodes.value)
                [
                  sm.set_next States.Switch_goal;
                ][
                  proc (write_array n_ways_to_goal ~idx:clearmemo_idx.value ~data:(zero 64));
                  clearmemo_idx <-- (clearmemo_idx.value +:. 1);
                ]
            ]);
          (States.Switch_goal,
            [
              nid_goal <-- goal_sel;
              proc (write_array n_ways_to_goal ~idx:goal_sel ~data:(one 64));
              savetomemo_idx <-- (num_nodes.value -:. 1); (* reverse topological top_order *)
              sm.set_next States.Switch_node;
            ]);
          (States.Switch_node,
            [
              dive_idx <-- cur_first_edge;
              dive_sum <--. 0;
              sm.set_next States.Dive;
            ]);
          (States.Dive,
            let head_node_cur = read_array e_head_node ~idx:dive_idx.value in
            let head_node_ways_to_goal = read_array n_ways_to_goal ~idx:head_node_cur in
            [
              if_ (cur_node ==: nid_goal.value)
                [
                  sm.set_next States.Save_to_memo;
                ][
                  if_ (dive_idx.value ==: nil_edge)
                    [
                      sm.set_next States.Save_to_memo;
                    ][
                      dive_sum <-- (dive_sum.value +: head_node_ways_to_goal);
                      dive_idx <-- (read_array e_next ~idx:dive_idx.value);
                    ]
                ];
            ]);
          (States.Save_to_memo,
            [
              when_ (cur_node <>: nid_goal.value)
                [
                  proc (write_array n_ways_to_goal ~idx:cur_node ~data:dive_sum.value);
                ];
              
              if_ (savetomemo_idx.value ==:. 0)
                [
                  sm.set_next States.Conclude_pass;
                ][
                  savetomemo_idx <-- (savetomemo_idx.value -:. 1);
                  sm.set_next States.Switch_node;
                ];
            ]);
          (States.Conclude_pass,
            [
              if_ (pass_idx.value ==:. 0)
                [
                  you_out <-- (read_array n_ways_to_goal ~idx:nid_you.value);
                  dac_out <-- (read_array n_ways_to_goal ~idx:nid_dac.value);
                  fft_out <-- (read_array n_ways_to_goal ~idx:nid_fft.value);
                ][
                  if_ (pass_idx.value ==:. 1)
                    [
                      svr_dac <-- (read_array n_ways_to_goal ~idx:nid_svr.value);
                      fft_dac <-- (read_array n_ways_to_goal ~idx:nid_fft.value);
                    ][
                      svr_fft <-- (read_array n_ways_to_goal ~idx:nid_svr.value);
                      dac_fft <-- (read_array n_ways_to_goal ~idx:nid_dac.value);
                    ];
                ];

              if_ (pass_idx.value ==:. 2)
                [
                  sm.set_next States.Conclude;
                ][
                  pass_idx <-- (pass_idx.value +:. 1);
                  clearmemo_idx <--. 0;
                  sm.set_next States.Clear_memo;
                ]
            ]);
          (States.Conclude,
            let svr_fft_dac_out = (svr_fft.value *^: fft_dac.value) *^: dac_out.value in
            let svr_dac_fft_out = (svr_dac.value *^: dac_fft.value) *^: fft_out.value in
            [
              p1_result <-- you_out.value;
              p2_result <-- svr_fft_dac_out +: svr_dac_fft_out;
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
    ];


  (* for waveform debugging: *)
  let _uart_value = uart_rx.value -- "uart_value" in
  let _uart_valid = uart_rx.valid -- "uart_valid" in
  let _st_idle = sm.is States.Idle -- "st_idle" in
  let _st_receive = sm.is States.Receive -- "st_receive" in
  let _st_find_node = sm.is States.Find_node -- "st_find_node" in
  let _st_add_node = sm.is States.Add_node -- "st_add_node" in
  let _st_init_top = sm.is States.Init_top -- "st_init_top" in
  let _st_pop_top = sm.is States.Pop_top -- "st_pop_top" in
  let _st_dive_top = sm.is States.Dive_top -- "st_dive_top_edges" in
  let _st_switch_node = sm.is States.Switch_node -- "st_switch_node" in
  let _num_nodes_value = num_nodes.value -- "num_nodes" in
  let _num_edges_value = num_edges.value -- "num_edges" in
  let _nid_you_value = nid_you.value -- "nid_you" in
  let _nid_out_value = nid_out.value -- "nid_out" in
  let _nid_svr_value = nid_svr.value -- "nid_svr" in
  let _nid_dac_value = nid_dac.value -- "nid_dac" in
  let _nid_fft_value = nid_fft.value -- "nid_fft" in
  let _st_dp_set_goal = sm.is States.Switch_goal -- "st_switch_goal" in
  let _you_out_value = you_out.value -- "you_out" in
  let _dac_out_value = dac_out.value -- "dac_out" in
  let _fft_out_value = fft_out.value -- "fft_out" in
  let _svr_dac_value = svr_dac.value -- "svr_dac" in
  let _fft_dac_value = fft_dac.value -- "fft_dac" in
  let _svr_fft_value = svr_fft.value -- "svr_fft" in
  let _dac_fft_value = dac_fft.value -- "dac_fft" in
  let _pass_idx_value = pass_idx.value -- "pass_idx" in

  let debug_output =
    _uart_valid
    |: (_uart_value >:. 0)
    |: _st_idle
    |: _st_receive
    |: _st_find_node
    |: _st_add_node
    |: _st_init_top
    |: _st_pop_top
    |: _st_dive_top
    |: _st_switch_node
    |: (_num_nodes_value >:. 0)
    |: (_num_edges_value >:. 0)
    |: (_nid_you_value >:. 0)
    |: (_nid_out_value >:. 0)
    |: (_nid_svr_value >:. 0)
    |: (_nid_dac_value >:. 0)
    |: (_nid_fft_value >:. 0)
    |: _st_dp_set_goal
    |: (_you_out_value >:. 0)
    |: (_dac_out_value >:. 0)
    |: (_fft_out_value >:. 0)
    |: (_svr_dac_value >:. 0)
    |: (_fft_dac_value >:. 0)
    |: (_svr_fft_value >:. 0)
    |: (_dac_fft_value >:. 0)
    |: (_pass_idx_value >:. 0)
  in

  p1_result.value, p2_result.value, (sm.is States.Done), debug_output
;;
