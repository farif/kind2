(* This file is part of the Kind 2 model checker.

   Copyright (c) 2014 by the Board of Trustees of the University of Iowa

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License.  You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0 

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License. 

*)

open Format
open Lib

module TS = TransSys

module SMT  : SolverDriver.S = GenericSMTLIBDriver


(* Declare a new function symbol *)
let declare_fun fmt uf =
  let fun_symbol = UfSymbol.name_of_uf_symbol uf in
  let arg_sorts = UfSymbol. arg_type_of_uf_symbol uf in
  let res_sort = UfSymbol.res_type_of_uf_symbol uf in
    fprintf fmt
      "@[<hv 1>(declare-fun@ %s@ @[<hv 1>%s@]@ %s)@]\n@." 
      fun_symbol
      (paren_string_of_string_list (List.map SMT.string_of_sort arg_sorts))
      (SMT.string_of_sort res_sort)


(* Define a new function symbol as an abbreviation for an expression *)
let define_fun fmt fun_symbol arg_vars res_sort defn = 
  fprintf fmt
  "@[<hv 1>(define-fun@ %s@ @[<hv 1>(%a)@]@ %s@ %a)@]\n@." 
  (UfSymbol.string_of_uf_symbol fun_symbol)
  (pp_print_list
     (fun ppf var -> 
        Format.fprintf ppf "(%s %s)" 
          (Var.string_of_var var)
          (SMT.string_of_sort (Var.type_of_var var)))
     "@ ")
  arg_vars
  (SMT.string_of_sort res_sort)
  SMT.pp_print_expr defn


(* Assert the expression *)
let assert_expr fmt expr = 
  fprintf fmt
    "@[<hv 1>(assert@ @[<hv>%s@])@]\n@." 
    (SMT.string_of_expr expr)


let push fmt = fprintf fmt "@[<hv 1>(push 1)@]\n@." 

let pop fmt = fprintf fmt "@[<hv 1>(pop 1)@]\n@." 

let check_sat fmt = fprintf fmt "@[<hv 1>(check-sat)@]\n@." 

let sexit fmt = fprintf fmt "@[<hv 1>(exit)@]\n@." 



let create_dir dir =
  try if not (Sys.is_directory dir) then failwith (dir^" is not a directory")
  with Sys_error _ -> Unix.mkdir dir 0o755



let global_certificate sys =
  let certs, props = List.fold_left (fun ((c_acc, p_acc) as acc) -> function
      | p, TS.PropInvariant c -> c :: c_acc, p :: p_acc
      | _ -> Event.log L_warn "Some properties are not valid";
        acc
    ) ([], []) (TS.get_properties sys) in

  let certs = List.fold_left (fun c_acc (_, c) ->
      c :: c_acc) certs (TS.get_invariants sys) in

  Term.mk_and props, Certificate.merge certs


let linestr = String.make 77 '-'

let print_line fmt = fprintf fmt ";; %s\n" linestr

let add_section fmt title =
  fprintf fmt "\n\n";
  print_line fmt;
  fprintf fmt ";; %s :\n" title;
  print_line fmt;
  fprintf fmt "@."
  




let echo fmt s = fprintf fmt "(echo \"%s\")@." (String.escaped s)


type s_info = {
  (* s_prop : Term.t; *)
  s_trans : Term.t;
  s_phi : Term.t;
}


let generate_certificate sys =

  let dirname = "." in

  create_dir dirname;

  let certificate_filename = 
    Filename.concat
      dirname
      (Format.sprintf "%s.certificate.smt2" 
         (Filename.basename (Flags.input_file ()))
      )
  in

  let certif_oc = open_out certificate_filename in
  
  let fmt = formatter_of_out_channel certif_oc in

  let prop, (k, phi) = global_certificate sys in
  
  (* Declaring state variables upto k *)
  add_section fmt "State variables";
  TS.declare_vars_of_bounds
    sys (declare_fun fmt) Numeral.zero (Numeral.of_int (k + 2));

  (* Declaring function symbols *)
  add_section fmt "Function symbols";
  List.iter (fun (f, (a, d)) ->
      define_fun fmt f a Type.t_bool d) (TS.uf_defs sys);

  
  (* Declaring initial state *)
  add_section fmt "Initial states";
  let init_n = "__I__" in
  let init_s = UfSymbol.mk_uf_symbol init_n [] Type.t_bool in
  define_fun fmt init_s [] Type.t_bool (TS.init_term sys);
  let init_t = Term.mk_uf init_s [] in
  
  (* Declaring property *)
  add_section fmt "Original property";
  let prop_n = "__P__" in
  let prop_s = UfSymbol.mk_uf_symbol prop_n [] Type.t_bool in
  define_fun fmt prop_s [] Type.t_bool prop;
  let prop_t = Term.mk_uf prop_s [] in
  
  
  let symbols_step = Hashtbl.create k in 
  let trans_defs = ref [] in
  let phi_defs = ref [] in
  
  for i = 0 to k + 1 do

    let ni = Numeral.of_int i in
    let nip1 = Numeral.of_int (succ i) in

    let ti = TransSys.trans_fun_of sys ni nip1 in
    let tn = sprintf "__T__%d_%d" i (succ i) in
    let ts = UfSymbol.mk_uf_symbol tn [] Type.t_bool in
    let s_trans = Term.mk_uf ts [] in
    
    let phii = Term.bump_state ni phi in
    let phin = sprintf "__PHI__%d" i in
    let phis = UfSymbol.mk_uf_symbol phin [] Type.t_bool in
    let s_phi = Term.mk_uf phis [] in

    trans_defs := (ts, ti) :: !trans_defs;
    phi_defs := (phis, phii) :: !phi_defs;
    Hashtbl.add symbols_step i { s_trans; s_phi }
    
  done;

  
  (* Declaring transition steps *)
  add_section fmt "Transition_relation (steps)";
  List.iter
    (fun (f, d) -> define_fun fmt f [] Type.t_bool d)
    (List.rev !trans_defs);

  
  (* Declaring k-inductive invariant *)
  add_section fmt (sprintf "%d-Inductive invariant (steps)" k);
  List.iter
    (fun (f, d) -> define_fun fmt f [] Type.t_bool d)
    (List.rev !phi_defs);


  add_section fmt "CERTIFICATE CHECKER";

  (* Checking base case *)
  add_section fmt "Base case";
  echo fmt "Checking base case:";
  push fmt;
  assert_expr fmt init_t;
  for i = 0 to k do
    push fmt;
    echo fmt (string_of_int i);
    
    (* unroll i times*)
    let l = ref [] in
    for j = 0 to i do
      l := (Hashtbl.find symbols_step j).s_trans :: !l
    done;

    if !l <> [] then assert_expr fmt (Term.mk_and (List.rev !l));
    
    let phi_i = (Hashtbl.find symbols_step i).s_phi in
    assert_expr fmt (Term.mk_not phi_i);
    check_sat fmt;
    pop fmt;
  done;
  pop fmt;
  

  (* Checking base case *)
  add_section fmt (sprintf "%d-Inductiveness" k);
  echo fmt (sprintf "Checking %d-inductive case:" k);
  push fmt;
  (* unroll k times*)
  let l = ref [] in
  for i = 0 to k do
    let { s_trans = ti; s_phi = pi} = Hashtbl.find symbols_step i in
    l := ti :: pi :: !l
  done;

  let { s_trans = tkp1; s_phi = phikp1 } = Hashtbl.find symbols_step (succ k) in
  assert_expr fmt (Term.mk_and (List.rev (tkp1 :: !l)));
  assert_expr fmt (Term.mk_not phikp1);
  check_sat fmt;
  pop fmt;

  
  (* Checking implication *)
  add_section fmt "Property subsumption";
  echo fmt "Checking property subsumption";
  push fmt;
  let phi0 = (Hashtbl.find symbols_step 0).s_phi in
  assert_expr fmt phi0;
  assert_expr fmt (Term.mk_not prop_t);
  check_sat fmt;
  pop fmt;
  sexit fmt;


  (* Close file *)
  close_out certif_oc;

  printf "Certificate was written in %s@." certificate_filename
