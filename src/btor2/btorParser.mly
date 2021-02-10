%{ 
  open Lib
  open BtorAst
  open BtorExpr
  
(* module A = BtorAst *)

  exception ParserError of string

let mk_pos = position_of_lexing 

(*    exception ParserError of string *)

let str_opt_int x = 
  match x with
  Some v -> Some (int_of_string v)
  | None -> None

let to_bool = function
      '0' -> false
    | '1' -> true
    | _ -> raise (ParserError "Undefined boolean value")

let rec bool_list ch = 
    match ch with
    | "" -> []
    | ch -> to_bool (String.get ch 0 ) :: (bool_list (String.sub ch 1 ( (String.length ch)-1) ) )

%}

(*%token <int> Int*)
%token <string> Num

%token <string> ID

%token NL

(* node *)
%token STATE INIT NEXT

(* I/O *)
%token INPUT OUTPUT

(* properties under consideration *)
%token CONSTRAINT BAD (*FAIR JUSTICE*)

(* Type definitions *)
%token SORT BITVEC ARRAY

(* constants *)
%token ZERO ONE ONES 
%token CONSTBIN 
%token CONSTDEC 
%token CONSTHEX

(* array operations *)
%token WRITE READ

(* arithmetic operations *)
%token ADD MUL SUB UDIV UREM
%token SDIV SMOD SREM

(* Unsigned/Signed inequality *) 
%token UGT UGTE ULT ULTE 
%token SGT SGTE SLT SLTE

(* Boolean *)
%token NOT AND NAND NOR OR XNOR XOR

%token NEQ EQ IMPLIES IFF 
%token ITE

%token NEG
%token REDAND REDOR REDXOR  

%token ROL ROR SLL SRA SRL

%token SLICE UEXT SEXT CONCAT

(* Overflow *)
%token UADDO SADDO UDIVO SDIVO UMULO SMULO USUBO SSUBO

%token EOF

%start main
%type <BtorAst.btor> main

%%

main: bp = nodes EOF { Btor2 bp } (*Nodes bp*)
        
nodes: n = pnode {[n]}
    | n = pnode m = nodes { n :: m }
    
pnode: nid = Num n = node id = option(ID) NL { Node(mk_pos $startpos, nid, n, id) }
    | sid = Num s = sort NL { Sort(sid, s) }

sort_id: sid = Num {Sid sid}

sort:  SORT BITVEC uint = Num { Bitvec(int_of_string uint) }
    | SORT ARRAY s1 = Num s2 = Num { Array(Sid s1, Sid s2) }
(*    | sid = Int BOOL b = Int { Bool bool_of_int b} 
print_string ("NID " ^ nid ^ "\n");
print_string ("Nid Condition:"^ nid^"\n"); 
*)

node_id: nid = Num { Nid  nid } 

node:
      INPUT sid = sort_id  { Input(sid) } (*later deal with ID option*)
    | STATE sid = sort_id  { State(sid) } (*later deal with ID option*)
    | INIT sid = sort_id n1 = node_id n2 = node_id  { Init(sid, n1, n2) } (*later deal with ID option*)
    | NEXT sid = sort_id n1 = node_id n2 = node_id  { Next(sid, n1, n2) } (*later deal with ID option*)
 
    | op = uop sid = sort_id nid = node_id { Uop(sid, op, nid) }
    | op = bop sid = sort_id n1 = node_id n2 = node_id  { Bop(sid, op, n1, n2) }
    | op = top sid = sort_id n1 = node_id n2 = node_id n3 = node_id  {Top(sid, op, n1, n2, n3) }
    | op = opidx sid = sort_id n1 = node_id uint1 = Num uint2 = option(Num) { Idx(sid, op, n1, (int_of_string uint1), str_opt_int uint2) }

    | ONE sid = sort_id { One(sid) }
    | ONES sid = sort_id { Ones(sid)}
    | ZERO sid = sort_id { Zero(sid) }

    | CONSTBIN sid = sort_id bin = Num { Constbin(sid,  bool_list bin) } (* Correct default boolean, Num is Boolean 0 or 1 *)
    | CONSTDEC sid = sort_id dec = Num { Constdec(sid, (int_of_string dec)) } (* Num is Integer *)
    | CONSTHEX sid = sort_id hex = Num { Consthex(sid,  hex) } (* Num is Hexadecimal *)

    | BAD nid = node_id  { Bad (nid) }
    | CONSTRAINT nid = node_id  { Constraint (nid) }
    | OUTPUT nid = node_id  { Output (nid) }

 uop: NOT { Not }
    | NEG { Neg }
    | REDAND { Redand }
    | REDOR { Redor }
    | REDXOR  {Redxor } 

 bop: AND { And }
    | NAND { Nand }
    | NOR { Nor }
    | OR { Or }
    | XOR { Xor }
    | XNOR { Xnor }
    | IMPLIES { Implies}
    | IFF { Iff }

    | EQ { Eq }
    | NEQ { Neq }

    | UGT { Ugt }
    | UGTE {Ugte}
    | SGT { Sgt }
    | SGTE {Sgte}
    | ULT { Ult }
    | SLT { Slt }
    | ULTE { Ulte }
    | SLTE { Slte }

    | ROL { Rol }
    | ROR { Ror }
    | SLL { Sll }
    | SRA { Sra }
    | SRL { Srl }

    | ADD { Add }
    | SUB { Sub }
    | MUL { Mul }
    | UDIV { Udiv }
    | SDIV { Sdiv }
    | SMOD { Smod }
    | UREM { Urem }
    | SREM { Srem}

    | UADDO { Uaddo }
    | SADDO { Saddo }
    | UDIVO { Udivo }
    | SDIVO { Sdivo }
    | UMULO { Umulo }
    | SMULO { Smulo }
    | USUBO { Usubo }
    | SSUBO { Ssubo}
    
    | READ { Read }
    | CONCAT { Concat }

 top: ITE { Ite }
    | WRITE { Write }

 opidx: SLICE { Slice }
    | UEXT { Uext }
    | SEXT { Sext }