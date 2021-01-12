open BtorAst

exception Failure of string

(*  abstract type of contexts 
    type 'a ctx = (string * 'a) list 
*)

type 'a ctx = (string * 'a) list 

let rec lookup x ctx =
  match ctx with
  | [] -> failwith (x ^ " not found")
  | (y, v) :: r -> if x=y then v else lookup x r

let empty_ctx = []

let rec lookup_sort sid (env : pnode list) =
  match env with 
  | [] -> raise (Failure (" sort id " ^  sid ^ " not found!"))
  | h :: t -> match h with
      Sort(sid1, sort) -> if sid = sid1 then sort else lookup_sort sid t
    | Node(_, _, _) -> lookup_sort sid t                    

let rec lookup_node nid (env : pnode list) =
  match env with 
  | [] -> raise (Failure (" node id " ^  nid ^" not found!"))
  | h :: t -> match h with
      Node(nid1, node, _) -> if nid = nid1 then node else lookup_node nid t
    |  Sort(_, _)-> lookup_node nid t                    

let filter_sorts v l =
  match v with
    Sort(sid, s) -> (sid, s) :: l
  | _ -> l

let filter_nodes v l =
  match v with
    Node(nid, n, _) -> (nid, n) :: l
  | _ -> l

let sortmap(prog :btor) =
  match prog with Btor2 nodes ->  List.fold_right filter_sorts nodes []

let nodemap(prog :btor) =
  match prog with Btor2 nodes ->  List.fold_right filter_nodes nodes []

