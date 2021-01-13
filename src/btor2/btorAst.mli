open BtorExpr

exception Parser_error

type num = string
type uint = int

type sort = 
    Sid of num 
  | Bitvec of uint 
  | Array of sort * sort

(* hexadecimal, change to hex*)

type node =
    Nid of num
  | Input of sort 
  | State of sort 
  | Init of sort * node * node
  | Next of sort * node * node
  (* Expression *)
  | Uop of sort * uop * node
  | Bop of sort * bop * node * node
  | Top of sort * top * node * node * node
  | Idx of sort * opidx * node * uint * uint option
  (* Constants *)
  | One of sort
  | Ones of sort
  | Zero of sort
  | Constbin of sort * bool list
  | Constdec of sort * int (* unsigned integer *)
  | Consthex of sort * string
  (* Property *)
  | Bad of node 
  | Constraint of node 
  | Output of node

type pnode = 
    Node of num * node * string option 
  | Sort of num * sort

type btor = Btor2 of pnode list

(* Btor Type *)
type btor_type = 
    BV of uint
  | AR of btor_type * btor_type

(* Pretty Printing *)                                                                                                                                                                                
type pfp = Format.formatter 

val pp_string :
  pfp -> (unit, pfp, unit) format -> unit -> unit
val pp_nl : pfp -> unit -> unit
val pp_bool : pfp -> bool -> unit
val pp_int : pfp -> int -> unit
val pp_str : pfp -> string -> unit
val pp_opt_str : pfp -> string option -> unit
val pp_opt_uint : pfp -> int option -> unit
val pp_num : pfp -> num -> unit
val pp_sort : pfp -> sort -> unit
val pp_uop : pfp -> uop -> unit
val pp_bop : pfp -> bop -> unit
val pp_top : pfp -> top -> unit
val pp_idx : pfp -> opidx -> unit
val pp_node : pfp -> node -> unit
val pp_pnode : pfp -> pnode -> unit
val pp_nodes : pfp -> pnode list -> string
val pp_sorts : pfp -> (string * sort) list -> string
val pp_nids : pfp -> (string * node) list -> string
(*val pp_opt_nodes : pfp -> pnode list option -> string*)
val pp_btor : pfp -> btor -> string
