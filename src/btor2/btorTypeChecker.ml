open BtorAst
open BtorContext

module R = Res
module LA = BtorAst

type 'a tc_result = ('a, string) result

type tc_type  = LA.btor_type
type tc_context = LA.pnode list
let type_error err = R.error ("Type error: " ^ err)

(*let typbool = BV 1*)

let rec infer_type_sort: tc_context -> LA.sort -> tc_type
  = fun ctx  -> function
    | Sid sid -> infer_type_sort ctx (lookup_sort sid ctx) 
    | Bitvec n -> BV n
    | Array (s1,s2) -> AR (infer_type_sort ctx s1, infer_type_sort ctx s2)
       
let rec infer_type_node: tc_context -> LA.node -> tc_type tc_result 
  = fun ctx -> function
  |  Nid nid -> infer_type_node ctx (lookup_node nid ctx)
  | Input(sid) -> R.ok (infer_type_sort ctx sid) 
  | State(sid) -> R.ok (infer_type_sort ctx sid) 

  | Init (sid, n1, n2) -> let typ = infer_type_sort ctx sid in 
                          if  (infer_type_node ctx n1 =  infer_type_node ctx n1) then 
                          R.ok typ else 
                          type_error ("Init error: ")

  | Next(sid, n1, n2) -> let typ = infer_type_sort ctx sid in 
                          if  (infer_type_node ctx n1 =  infer_type_node ctx n1) then R.ok typ 
                          else type_error ("Next error: ")

  | Uop(sid, uop, n) -> let typ = infer_type_sort ctx sid in
                          let ntyp = infer_type_node ctx n in
                            if R.ok typ = ntyp then R.ok typ 
                            else type_error ("Next error: ")

                         (* match uop with
                             Not -> if typ = ntyp then R.ok typ 
                             else type_error ("Not expression failed")
                            | Neg -> if typ = ns then R.ok typ 
                             else type_error ("Neg expression failed")
                            | Redand -> if typ = typbool then R.ok typ 
                              else type_error ("Redand expression failed")
                            | Redor -> if typ = typbool then R.ok typ 
                              else type_error ("Redor expression failed")
                            | Redxor -> if typ = typbool then R.ok typ 
                              else type_error ("Redxor expression failed")
                          *)

  | Bop(sid, bop, n1, n2) ->  R.ok (infer_type_sort ctx sid) 
                              
  | Top(sid, top, n1, n2, n3) -> R.ok (infer_type_sort ctx sid) 
                                  (*match top with
                                    Ite ->  if typ = typbool && typ = n2typ && typ = n3typ then typ 
                                    else type_error ("ITE expression failed")
                                   | Write -> if typ = n2typ && typ = n3typ then typ 
                                    else type_error ("Write expression failed")
                                  *)        
                                  

  | Idx(sid, opidx, n, u1, u2) -> R.ok (infer_type_sort ctx sid)
  | One(sid) -> R.ok (infer_type_sort ctx sid)
  | Ones(sid) -> R.ok (infer_type_sort ctx sid)
  | Zero(sid) -> R.ok (infer_type_sort ctx sid)
  | Constbin(sid, bv) -> R.ok (infer_type_sort ctx sid) 
  | Constdec(sid, dec) -> R.ok (infer_type_sort ctx sid)
  | Consthex (sid, hex) -> R.ok (infer_type_sort ctx sid)
  | Bad n -> infer_type_node ctx n
  | Constraint n -> infer_type_node ctx n
  | Output n -> infer_type_node ctx n 
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
let rec infer_type_pnode: tc_context -> LA.pnode -> tc_type tc_result 
= fun ctx -> function
    Node(nid, n, id) -> infer_type_node ctx n
  | Sort(sid, s) -> R.ok (infer_type_sort ctx s) 


let rec check_nodes ctx prog = 
      infer_type_pnode ctx prog 
              
let typbtor prog env  =
  match prog with 
  | Btor2 pnodes -> check_nodes pnodes env 

let typ_check_btor prog =
    prog empty_ctx