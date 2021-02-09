(* ****************************************************** *)
(* Create transition system                               *)
(* ****************************************************** *)
module LA = BtorAst
module TS = TransSys
module TC = BtorTypeChecker

module A = Analysis
module P = Property

(* Create transition system *)

module SVS = StateVar.StateVarSet
module SVM = StateVar.StateVarMap
module SCM = Scope.Map

(* Hash map from node scopes to their index for fresh state variables.
   Used to make sure fresh state variables are indeed fresh after a restart,
   without risking to reach [MAXINT]. *)
let scope_index_map = ref SCM.empty
(* Returns a fresh index for a scope. *)
let index_of_scope s =
  let curr =
    try !scope_index_map |> SCM.find s with Not_found -> 0
  in
  scope_index_map := !scope_index_map |> SCM.add s (curr + 1) ;
  curr

(* Transition system and information needed when calling it *)
type node_def = {
  (* Node the transition system was created from *)
  node : BtorAst.btor;

  (* Initial state predicate *)
  init_uf_symbol : UfSymbol.t;

  (* Transition relation predicate *)
  trans_uf_symbol : UfSymbol.t;

  (* Transition system for node *)
  trans_sys : TransSys.t;

  (* Stateful local variables to be instantiated by the caller 

     Local variables of the callees of the node *)
  stateful_locals : StateVar.t list;

  (* Init flags to be set to true *)
  init_flags : StateVar.t list;

  (* Properties to be instantiated by the caller 

     Properties of the callees of the node *)
  properties : P.t list;
}
(*let v_x = Var.mk_state_var "x" Type.Int*)


let getpnodes = function
  | LA.Btor2 prog -> prog

let extract_vid id =
  match id with
  | Some var_id -> var_id
  | None -> "var_id"

(*
let get_nodetype ctx n = 
    let s = TC.infer_type_sort ctx n in 
    match s with
    | BV n -> n
    | _ ->  failwith "Unknown Type"
get_nodetype [v] s*)
let extract_nodes v l =
    match v with
    | LA.Node (_, n, id)-> 
        (match n with 
             LA.State s -> (StateVar.mk_state_var (extract_vid id)  [] (Type.mk_type (Type.BV (8)))) :: l
            |     _ -> l
        )
    | _ -> l

let filter_nodes subsystem =
    match subsystem with
    | LA.Btor2 pnodes -> List.fold_right extract_nodes pnodes []


(* let trans_sys_of_nodes
    ?(preserve_sig = false)
    ?(slice_nodes = false)
    subsystem analysis_param
  =

  let { A.top; A.abstraction_map; A.assumptions } =
    A.info_of_param analysis_param
  in
  (* Make sure top level system is not abstract

     Contracts would be trivially satisfied otherwise *)
  ( match analysis_param with
    | A.Interpreter _
    | A.ContractCheck _ -> ()
    | _ -> if A.param_scope_is_abstract analysis_param top then raise (
      Invalid_argument
        "trans_sys_of_nodes: Top-level system must not be abstract"
    )
  );
  let top_name = "TestNode" in

  (* TODO: Find top subsystem by name *)
  let pnodes =  getpnodes subsystem in

  let { trans_sys } =   
    
    try 

    let init_state = StateVar.mk_state_var "X" [] (Type.mk_int ()) 
    
    in
        let scope = Scope.mk_scope [] in
      (* Create a transition system for each node 
      trans_sys_of_node'*)
      TransSys.mk_trans_sys
        scope
        None
        init_state
        []
        []
        analysis_param
        I.Map.empty
        [] 
        pnodes (*nodes*)
        [top_name]

    (* Transition system must have been created *)
    with Not_found -> assert false

  in
  
  ( match analysis_param with
    | A.Refinement (_,result) ->
      (* The analysis that's going to run is a refinement. *)
      TransSys.get_prop_status_all_nocands result.A.sys
      |> List.iter (function
        | name, P.PropUnknown -> (* Unknown is still unknown, do nothing. *)
          ()
        
        | name, (P.PropKTrue k as status) -> (* K-true is still k-true. *)
          TransSys.set_prop_status trans_sys name status
        
        | name, P.PropInvariant cert -> (* Invariant is still invariant. *)
          TransSys.set_prop_invariant trans_sys name cert;
          (* Adding to invariants of the system. *)
          let t = TransSys.get_prop_term trans_sys name in
          TransSys.add_invariant trans_sys t cert
          |> ignore
        
        | name, P.PropFalse cex -> (
          match P.length_of_cex cex with
          | l when l > 1 -> (* False at k>0 is now (k-1)-true. *)
            (* Minus 2 because l = k + 1. *)
            TransSys.set_prop_status trans_sys name (P.PropKTrue (l-2))
          | _ -> (* False at 0 is now unknown, do nothing. *)
            ()
        )
      )
    | _ -> ()
  ) ;

  (* Reset garbage collector to its initial settings *)
  Lib.reset_gc_params ();

  trans_sys *)

(* let  btor_to_trans prog =
    let statevars = filter_nodes prog in
        let trans_sys =
        {state_vars = statevars} 
         in trans_sys *)

(*
let node_name = "btor_eg" in

    let trans_sys = 
    TransSys.mk_trans_sys 
        [I.string_of_ident false node_name]
        None (* instance_state_var *)
        init_flag
        [] (* global_state_vars *)
        (signature_state_vars)
        globals.G.state_var_bounds
        global_consts
        ufs
        init_uf_symbol
        init_formals
        (Term.mk_and init_terms)
        trans_uf_symbol
        trans_formals
        (Term.mk_and trans_terms)
        None (* subsystems *)
        []
        None (* mode_requires *)
        None (* node_assumptions *)

*)

(*
let num = Term.mk_num_of_int 
let dec = Term.mk_dec_of_float 
let u_i s = UfSymbol.mk_uf_symbol s [Type.Int] Type.Int 
let u_b s = UfSymbol.mk_uf_symbol s [Type.Int] Type.Bool 
let var_i s t = Term.mk_uf (u_i s) [t] 
let varn_i s i = Term.mk_uf (u_i s) [num i] 
let var_b s t = Term.mk_uf (u_b s) [t] 
let varn_b s i = Term.mk_uf (u_b s) [num i] 
let pt = Term.mk_true ()
let pf = Term.mk_false ()
let eq l r = Term.mk_eq [l; r] 
let ite p l r = Term.mk_ite p l r 
let c j1 j2 = Term.mk_and [j1; j2]
let d j1 j2 = Term.mk_or [j1; j2]
let n j = Term.mk_not j
let leq l r = Term.mk_leq [l; r] 
let lt l r = Term.mk_lt [l; r] 
let geq l r = Term.mk_geq [l; r] 
let gt l r = Term.mk_gt [l; r] 
let plus a b = Term.mk_plus [a; b] 
let minus a b = Term.mk_minus [a; b] 
let times a b = Term.mk_times [a; b] 

let v_x = Var.mk_state_var "x" Type.Int  
let v_y = Var.mk_state_var "y" Type.Int  

let v_x0 = Var.mk_state_var_instance v_x 0
let v_x1 = Var.mk_state_var_instance v_x 1

let v_y0 = Var.mk_state_var_instance v_y 0
let v_y1 = Var.mk_state_var_instance v_y 1

let t_x0 = Term.mk_var v_x0 
let t_y0 = Term.mk_var v_y0 
let t_x1 = Term.mk_var v_x1 
let t_y1 = Term.mk_var v_y1 


let main () =
  
  let ts = 
    [ Term.mk_let [v_y0, (num 2)] (plus t_x0 t_y0),  [(v_x0, t_y0); (v_y0, (num 0))] 
    ]
  in
       

  List.iter 
    (function t, e -> 
      Format.printf "%a@." Term.pp_print_term t;
      let t' = term_of_value (eval_term t e) in
      Format.printf "%a@." Term.pp_print_term t')
    ts
  

;;

main ()


*)
(*

let main () = 

  let t_t = Term.mk_true () in
  let t_f = Term.mk_false () in

  let t_1f = Term.mk_dec_of_float 1. in
  let t_2f = Term.mk_dec_of_float 2. in
  let t_3f = Term.mk_dec_of_float 3. in

  let t_1i = Term.mk_num_of_int 1 in
  let t_2i = Term.mk_num_of_int 2 in
  let t_3i = Term.mk_num_of_int 3 in

  let t = Term.mk_is_int t_1f in

  let e = eval_term t in

  Format.printf 
    "%a %a %B@."
    Term.pp_print_term t
    Term.pp_print_term (term_of_value (e []))
    (bool_of_value (e []));
  


  let u1 = UfSymbol.mk_uf_symbol "P1" [] Type.Bool in
  let u2 = UfSymbol.mk_uf_symbol "P2" [] Type.Bool in
  let u3 = UfSymbol.mk_uf_symbol "P3" [] Type.Bool in
  let a = List.map (function s -> Term.mk_uf s []) [u1; u2; u3] in
  let t = Term.mk_eq a in
 

  Format.printf 
    "%a@."
    Term.pp_print_term t;

  let e = eval_term t in

  let envs = 
    [List.combine a [t_t; t_t; t_t];
     List.combine a [t_t; t_t; t_f];
     List.combine a [t_t; t_f; t_t];
     List.combine a [t_t; t_f; t_f];
     List.combine a [t_f; t_t; t_t];
     List.combine a [t_f; t_t; t_f];
     List.combine a [t_f; t_f; t_t];
     List.combine a [t_f; t_f; t_f]]
  in

  Format.printf 
    "@[<v>%a@]@."
    (pp_print_list Term.pp_print_term "") 
    (List.map term_of_value (List.map e envs));

  let u1 = UfSymbol.mk_uf_symbol "u1" [] Type.Int in

  let u2 = UfSymbol.mk_uf_symbol "u2" [Type.Int] Type.Int in

  let t1 = Term.mk_uf u1 [] in

  let t2 = 
    Term.mk_let 
      [("x", Term.mk_num_of_int 0)] 
      (Term.mk_uf u2 [Term.mk_sym "x" Type.Int]) in
  let t2' = Term.mk_uf u2 [Term.mk_num_of_int 0] in

  let e1 = eval_term t1 in


  Format.printf 
    "%a %a@." 
    Term.pp_print_term 
    t1
    Term.pp_print_term 
    (term_of_value (e1 [(t1, Term.mk_num_of_int 0)]));


  Format.printf 
    "%a@." 
    Term.pp_print_term 
    t2;
  
  let e2 = 
    eval_term t2 [(t1, Term.mk_num_of_int 0); (t2', Term.mk_num_of_int 1)] 
  in

  Format.printf 
    "%a@." 
    Term.pp_print_term 
    (term_of_value e2);


  let e2 = eval_subterms t2 in

  let env = [(t1, Term.mk_num_of_int 0); (t2', Term.mk_num_of_int 1)] in

  Term.TermHashtbl.iter 
    (fun k v -> 
      Format.printf 
        "%a %a@."
        Term.pp_print_term k
        Term.pp_print_term (try term_of_value (v env) with Invalid_argument _ -> k))
    e2

;;

main ()

*)