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

val typsort : sort -> pnode list -> sort 

(*val typ_node : node -> pnode list -> sort*)

val typnode : node -> pnode list -> node

val typpnode : pnode -> pnode list -> pnode

val typpnodes : pnode list -> pnode list -> pnode list

val typbtor : btor -> pnode list -> btor