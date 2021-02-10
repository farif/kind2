(* ****************************************************** *)
(* Create transition system                               *)
(* ****************************************************** *)
module LA = BtorAst
module TS = TransSys
module TC = BtorTypeChecker
module I = BtorIdent
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
    | LA.Node (_,_, n, id)-> 
        (match n with 
             LA.State s -> (StateVar.mk_state_var (extract_vid id)  [] (Type.mk_type (Type.BV (8)))) :: l
            |     _ -> l
        )
    | _ -> l

let filter_nodes subsystem =
    match subsystem with
    | LA.Btor2 pnodes -> List.fold_right extract_nodes pnodes []


let trans_sys_of_nodes
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
  
  let top_name = "TestNode" 
  in
  (* TODO: Find top subsystem by name *)
  let pnodes =  subsystem 
  in
   
    let init_state = StateVar.mk_state_var "X" [] (Type.mk_int ())     
      in

  (* Get types of state variables*)

  (* Usefull instances of state variables *)
    let init_uf_symbol = 
      UfSymbol.mk_uf_symbol
        "TestSymbol" 
        [Type.t_bool]
        Type.t_bool 
        in

    let trans_uf_symbol = 
      UfSymbol.mk_uf_symbol
        "TestSymbol" 
        [Type.t_bool]
        Type.t_bool 
        in
      let init_args = []
      in
        let trans_args = []
        in
        let init_term = Term.t_false
      in
        let trans_formals = Term.t_true
        in
        let scope = Scope.mk_scope [] in
      (* Create a transition system for each node 
      trans_sys_of_node'*)
      let trans_sys_node = TransSys.mk_trans_sys
        scope
        None
        init_state
        []
        []
        (StateVar.StateVarHashtbl.create 3)
        []
        [] 
        init_uf_symbol
        init_args
        init_term
        trans_uf_symbol
        trans_args 
        trans_formals
      (* No subsystems, no properties *)        
         [] [] (None, []) (Invs.empty ())
     
    (* Transition system must have been created *)
  in
      let (trans_sys, _) = trans_sys_node 
      in 
      
      (* let trans_node = {
          node = pnodes;
          init_uf_symbol = init_uf_symbol;
          trans_uf_symbol = trans_uf_symbol;
          trans_sys = trans_sys;
           stateful_locals = [];
          init_flags = [];
          properties = [] }
    in *)
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
  (trans_sys, subsystem)
