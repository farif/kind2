open BtorAst

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

val typ_check_btor : ('a list -> 'b) -> 'b