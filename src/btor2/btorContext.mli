open BtorAst

(* 
type 'a ctx = (string * 'a) list 
val lookup : string -> (string * 'a) list -> 'a
*)

type 'a ctx = (string * 'a) list 

val empty_ctx : 'a list

val lookup : string -> (string * 'a) list -> 'a


val lookup_sort : string -> pnode list -> sort
val lookup_node : string -> pnode list -> node
val filter_sorts : pnode -> (string * sort) list -> (string * sort) list
val filter_nodes : pnode -> (string * node) list -> (string * node) list
val sortmap : btor -> (string * sort) list
val nodemap : btor -> (string * node) list