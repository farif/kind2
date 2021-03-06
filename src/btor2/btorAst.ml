open Lib
open BtorExpr
open Format

(*[TODO] Extend ADT with position argument *)
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
    Node of position * num * node * string option 
  | Sort of num * sort

type btor = Btor2 of pnode list

(* Btor Node Type *)
type btor_type = 
    BV of uint
  | AR of btor_type * btor_type

(* Pretty Printing *)
type pfp = Format.formatter 

let pp_string ppf s : unit -> unit = fun _ -> fprintf ppf s

let pp_nl ppf = pp_string ppf "@\n"

let pp_bool ppf (b : bool) : unit = fprintf ppf "%b" b

let pp_int ppf (i : int) : unit = fprintf ppf "%i" i

let pp_str ppf (s : string) : unit = fprintf ppf "%s" s

let rec pp_bool_list ppf (blist : bool list) : unit =
  match  blist with
  | [] -> fprintf ppf "" 
  | [ h ] -> fprintf ppf "%a" pp_bool h; pp_bool_list ppf [] 
  | h :: t -> fprintf ppf "%a, " pp_bool h; pp_bool_list ppf t

let pp_opt_str ppf opt : unit =
  match opt with Some v -> fprintf ppf "%a" pp_str v | None -> ()

let pp_opt_uint ppf opt : unit =
  match opt with Some v -> fprintf ppf "%a" pp_int v | None -> ()

let pp_num ppf (i : num) : unit = fprintf ppf "%s" i


(* let rec pp_type ppf (sid : btor_type) =
  match sid with
  | BV n -> fprintf ppf "BV(%i)" n
  | AR (t1, t2) -> fprintf ppf "Array (%a,%a)" pp_type t1 pp_type t2 *)

let rec pp_sort ppf (sid : sort) =
  match sid with
  | Sid i -> pp_str ppf i
  (*  | Bool b -> fprintf ppf "Boolean(%b)" b *)
  | Bitvec n -> fprintf ppf "sort bitvec %i" n
  | Array (sid1, sid2) -> fprintf ppf "sort Array (%a,%a)" pp_sort sid1 pp_sort sid2

let pp_uop ppf (op : uop) =
  match op with
  | Not -> fprintf ppf "not"
  | Neg -> fprintf ppf "neg"
  | _ -> fprintf ppf "Add Later..."

let pp_bop ppf (op : bop) =
  match op with
  | And -> fprintf ppf "and"
  | Eq -> fprintf ppf "eq"
  | Neq -> fprintf ppf "neq"
  | Add -> fprintf ppf "add"
  | _ -> fprintf ppf "Add Later..."

let pp_top ppf (op : top) =
  match op with Ite -> fprintf ppf "ite" | Write -> fprintf ppf "write"

let pp_idx ppf (op : opidx) =
  match op with
  | Slice -> fprintf ppf "slice"
  | Uext -> fprintf ppf "uext"
  | Sext -> fprintf ppf "sext"

let rec pp_node ppf (n : node) =
  match n with
  | Nid i -> fprintf ppf "%a" pp_num i

  | Input (sid) -> fprintf ppf "input %a"  pp_sort sid
  | State (sid) -> fprintf ppf "state %a"  pp_sort sid
  | Init (sid, n1, n2) ->
    fprintf ppf "init %a %a %a" pp_sort sid pp_node n1 pp_node n2 
  | Next (sid, n1, n2) ->
    fprintf ppf "next %a %a %a" pp_sort sid pp_node n1 pp_node n2 

  | Uop (sid, op1, n1) -> fprintf ppf "%a %a %a" pp_sort sid pp_uop op1 pp_node n1 
  | Bop (sid, op2, n1, n2) ->
    fprintf ppf "%a %a %a %a" pp_bop op2 pp_sort sid pp_node n1 pp_node n2 
  | Top (sid, op3, n1, n2, n3) ->
    fprintf ppf "%a %a %a %a %a" pp_top op3 pp_sort sid pp_node n1 pp_node n2 pp_node n3 
  | Idx (sid, opidx, n, u1, u2) ->
    fprintf ppf "%a %a %a %a %a" pp_idx opidx pp_sort sid pp_node n pp_int u1 pp_opt_uint u2 

  | One(sid) -> fprintf ppf "one %a" pp_sort sid
  | Ones(sid) -> fprintf ppf "ones %a" pp_sort sid
  | Zero(sid) -> fprintf ppf "zero %a" pp_sort sid
  | Constbin (sid, v) -> fprintf ppf "constbin %a %a" pp_sort sid pp_bool_list v
  | Constdec (sid, v) -> fprintf ppf "constdec %a %a" pp_sort sid pp_int v 
  | Consthex (sid, v) -> fprintf ppf "consthex %a %a" pp_sort sid pp_str v 

  | Bad n -> fprintf ppf "bad %a" pp_node n
  | Constraint n -> fprintf ppf "inv %a" pp_node n
  | Output n -> fprintf ppf "output %a" pp_node n

let pp_pnode ppf (n : pnode) =
  match n with
    Node (_,i, n, id) -> fprintf ppf "%a %a %a" pp_num i pp_node n pp_opt_str id
  | Sort(i, sid) -> fprintf ppf "%a %a" pp_num i pp_sort sid

let rec pp_sorts ppf sorts =
  match sorts with
  | [] -> ""
  | [ (_, s) ] ->
    fprintf ppf "%a\n" pp_sort s;
    pp_sorts ppf []
  | (_, s) :: t ->
    fprintf ppf "%a\n" pp_sort s;
    pp_sorts ppf t

let rec pp_nids ppf nodes =
  match nodes with
  | [] -> ""
  | [ (_, n) ] ->
    fprintf ppf "%a\n" pp_node n;
    pp_nids ppf []
  | (_, n) :: t ->
    fprintf ppf "%a\n" pp_node n;
    pp_nids ppf t

let rec pp_nodes ppf nodes =
  match nodes with
  | [] -> "\n"
  | [ h ] ->
    fprintf ppf "%a" pp_pnode h;
    pp_nodes ppf []
  | h :: t ->
    fprintf ppf "%a\n" pp_pnode h;
    pp_nodes ppf t
(*
let pp_opt_nodes ppf opt  =
  match opt with Some v -> pp_nodes ppf v | None -> " "
  *)
let pp_btor ppf (p : btor) =
  match p with Btor2 nodes -> pp_nodes ppf nodes
