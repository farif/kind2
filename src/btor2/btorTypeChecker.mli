module LA = BtorAst

type 'a tc_result = ('a, string) result
type tc_type  = LA.btor_type

(* 
  num unsigned integer > 0 
      |- i > 0
  ? ------------
    |- i : numT

   type_lookup(context, i) = numT
  -------------------------------
        context |- i : SidT

*)

(*
--------------------------
context | sid : context()
*)

(*
let rec t_node (n: typNode) (ctx : typsort ctx) =
  match n with
  | Sort(id, s) -> xtype = t_sort s ctx in 
                  (id, xtype) :: ctx  
  | _ ->  
 *)
type pfp = Format.formatter 


val typ_check_btor_prog : LA.btor -> tc_type tc_result list

val pp_tc_type: pfp -> tc_type -> unit