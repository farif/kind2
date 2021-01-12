open BtorAst
open BtorContext

(* 
open Ast.Exp

  num unsigned integer > 0 
      |- i > 0
  ? ------------
    |- i : numT

   type_lookup(context, i) = numT
  -------------------------------
        context |- i : SidT

*)

(*
              with 
                Not_found -> raise (TyperError "abc") 

--------------------------
context | sid : context()

let rec typ_node (n : node) (env :typsort ctx) =
  match n with
  | Nid nid -> let nidv = string_of_int nid in lookup (string_of_int nid) env
  | Input sid -> let ts = typsort sid env in InputT ts
  | State sid -> let ts = typsort sid env in StateT ts
  | Init (sid, n1, n2) -> let tsid = typsort sid env in
                          let ts1 = typ_node n1 env in
                          let ts2 = typ_node n2 env in  
                                InitT (tsid, ts1, n1, ts2, n2) 
  | Next (sid, n1, n2) -> let tsid = typsort sid env in
                        let ts1 = typ_node n1 env in
                        let ts2 = typ_node n2 env in  
                        Next (tsid, ts1, n1, ts2, n2)

*)
(*
let get_sid node = 
  match node with
  | Nid nid -> nid
  | _ -> failwith "No nid Found!!"   

let get_bvsize bv =
  match bv with
  | Bitvec n -> n
  | _ -> failwith "BV type expected"
*)

let typbool = Bitvec 1

let rec typsort s env =
  match s with
    Sid sid ->  let s = int_of_string sid in if s > 0 then typsort (lookup_sort sid env) env else raise(Failure ("Sid %s lower than zero <= 0" ^ sid)) (* (env :typsort ctx) let v = string_of_int i in lookup v env*)                 
  | Bitvec n ->  if n > 0 then  Bitvec n else  raise(Failure ("Bitvec num is invalid" ^ string_of_int n))
  | Array (sid1, sid2) -> let s1 = typsort sid1 env in 
                          let s2 = typsort sid2 env in 
                          Array(s1, s2)

let rec typ_node n env =
  match n with
    Nid nid -> typ_node (lookup_node nid env) env
  | Input sid -> typsort sid env 
  | State sid -> typsort sid env
  | Init (sid, _, _) ->  typsort sid env
  | Next (sid, _, _) -> typsort sid env
  | Uop (sid, _, _) -> typsort sid env
  | Bop (sid, _, _, _) -> typsort sid env
  | Top (sid, _, _, _, _) -> typsort sid env
  | Idx (sid, _, _, _, _) -> typsort sid env
  | One(sid) -> typsort sid env
  | Ones(sid) -> typsort sid env
  | Zero(sid) -> typsort sid env
  | Constbin (sid, _) -> typsort sid env
  | Constdec (sid, _) -> typsort sid env
  | Consthex (sid, _) -> typsort sid env
  | Bad n -> typ_node n env
  | Constraint n -> typ_node n env
  | Output n -> typ_node n env

let rec typnode n env =
  match n with
    Nid nid -> typnode (lookup_node nid env) env
  | Input(sid) -> let s = typsort sid env in 
                 Input s 
  | State(sid) -> let s = typsort sid env in 
                 State s
  | Init (sid, n1, n2) -> let s = typsort sid env in 
                          let n1 = typnode n1 env in
                          let n2 = typnode n2 env in
                          let ns1 = typ_node n1 env in
                          let ns2 = typ_node n2 env in
                          if s = ns1 && s = ns2 then Init(s, n1,  n2) else raise(Failure "Init ill-defined") 
  
  | Next(sid, n1, n2) -> let s = typsort sid env in 
                          let n1 = typnode n1 env in
                          let n2 = typnode n2 env in
                          let ns1 = typ_node n1 env in
                          let ns2 = typ_node n2 env in
                          if s = ns1 && s = ns2 then Next(s, n1,  n2) else raise(Failure "Next ill-defined") 

  | Uop(sid, uop, n) -> let s = typsort sid env in 
                         let n = typnode n env in
                         let ns = typ_node n env in
                         (match uop with
                             Not -> if s = ns then Uop(s, Not, n) else raise(Failure "Not ill-defined")
                            | Neg -> if s = ns then Uop(s, Neg, n) else raise(Failure "Neg ill-defined")
                            | Redand -> if s = typbool then Uop(s, uop, n) else raise(Failure "Redand ill-defined")
                            | Redor -> if s = typbool then  Uop(s, uop, n) else raise(Failure "Redor ill-defined")
                            | Redxor -> if s= typbool then  Uop(s, uop, n) else raise(Failure "Redxor ill-defined")
                          )

  | Bop(sid, bop, n1, n2) -> let s = typsort sid env in 
                              let n1 = typnode n1 env in
                              let n2 = typnode n2 env in
                              Bop(s, bop, n1,  n2)

  | Top(sid, top, n1, n2, n3) -> let s = typsort sid env in 
                                  let n1 = typnode n1 env in
                                  let n2 = typnode n2 env in
                                  let n3 = typnode n3 env in
                                  let ns1 = typ_node n1 env in
                                  let ns2 = typ_node n2 env in
                                  let ns3 = typ_node n3 env in
                                  (match top with
                                    Ite ->  if ns1 = typbool && s = ns2 && s = ns3 then Top(s, top, n1, n2, n3) else raise(Failure "ITE ill-defined")
                                   | Write -> Top(s, top, n1, n2, n3)
                                  )        
                                  

  | Idx(sid, opidx, n, u1, u2) -> let s = typsort sid env in 
                                   let n = typnode n env in
                                   Idx(s, opidx, n, u1, u2) 

  | One(sid) -> let s = typsort sid env in  
                One s  
  | Ones(sid) -> let s = typsort sid env in 
                Ones s
  | Zero(sid) -> let s = typsort sid env in
                Zero s
  | Constbin(sid, bv) -> let s = typsort sid env in 
                          Constbin(s, bv)
  | Constdec(sid, dec) -> let s = typsort sid env in
                          Constdec(s, dec)
  | Consthex (sid, hex) -> let s = typsort sid env in
                          Consthex(s, hex)
  | Bad n -> let n = typnode n env in
              Bad n
  | Constraint n -> let n = typnode n env in 
                    Constraint n
  | Output n -> let n = typnode n env in
                Output n
(*  and

typops sid top n1  n2 n3 env = 
  match top with
    Ite ->  let cond_op = int_of_string (get_sid n1) in 
            if  cond_op < 0 then
            let refnode = Nid(string_of_int (-1*cond_op)) in
                let newnode = Uop(sid, Neg, refnode) in
             Top(typsort sid env, top, typnode newnode env, typnode n2 env, typnode n3 env)
            else 
             Top(typsort sid env, top, typnode n1 env, typnode n2 env, typnode n3 env)
  | Write ->  Top(typsort sid env, top, typnode n1 env, typnode n2 env, typnode n3 env)

*)
let typpnode pn env =
  match pn with
    Node(nid, n, id) -> Node(nid, typnode n env, id)
  | Sort(sid, s) -> Sort(sid, typsort s env)

(*in (string_of_int sid, tv) :: env;  
  raise (TypeCheckingError "Error")
  (* Auxiliary function*)
*)

let rec typpnodes (nodeList) env  =
  match nodeList with 
  | [] -> []
  | h :: t ->  let n = typpnode h env in 
                n :: typpnodes t (n :: env)  

let typbtor prog env  =
  match prog with 
  | Btor2 pnodes -> let bt_nodes = typpnodes pnodes env in
    Btor2 bt_nodes

(*
let typ_opt_pnodes (opt_nodes) =
  match opt_nodes with
  | Some  pnodes -> typ_pnodes pnodes pnodes
  | None  -> []


let check_prog (prog :btor) (env :typ ctx)  =
   match prog with Btor2 
    nodes -> 
        match nodes with -> 
      | Some opt_nodes ->  Some check_pnodes nodes env
      | None -> None
  | None -> None   

let rec t_node (n: typNode) (ctx : typsort ctx) =
  match n with
  | Sort(id, s) -> xtype = t_sort s ctx in 
                  (id, xtype) :: ctx  
  | _ ->  

*)


(*typ_node n  env1 env2,



 *)
