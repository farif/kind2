{

  open BtorParser

  open Format
  open Lexing

  let get = Lexing.lexeme
  
  exception Lexer_error of string

  
  let error fmt = Printf.kprintf (fun msg -> raise (Lexer_error msg)) fmt

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = pos.pos_cnum;
                 pos_lnum = pos.pos_lnum + 1;
      }

  let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
    fprintf outx "%s near line %d, column%d" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


(* XML or plain text warning.

   Adrien: Relying on Event causes circular build. Factor in Lib, along with
     context warnings? *)
let print_warning fmt =
  if Flags.log_format_xml () then
    Format.printf ("@[<hov 2>\
        <Log class=\"warn\" source=\"parse\">@,\
          @[<hov>" ^^ fmt ^^ "@]\
        @;<0 -2></Log>\
      @]@.")
  else
    Format.printf ("%t @[<v>" ^^ fmt ^^ "@]@.") Pretty.warning_tag

(* Pretty-print an array of integers *)
let rec pp_print_int_array i ppf a = 

  if i = 0 then 
    Format.fprintf ppf "@[<hv 3>[|";
  
  if i >= Array.length a then

    Format.fprintf ppf "|]@]"

  else

    ( 

      Format.fprintf ppf "%d" a.(i);

      if i+2 = Array.length a then 
        Format.fprintf ppf ";@ ";
      
      pp_print_int_array (succ i) ppf a
    
    )

(* Pretty-print a position *)
let pp_print_position ppf 
    { Lexing.pos_fname;
      Lexing.pos_lnum;
      Lexing.pos_bol;
      Lexing.pos_cnum } =

  Format.fprintf ppf 
    "@[<hv 2>{ pos_fname : %s;@ \
     pos_lnum : %d;@ \
     pos_bol : %d;@ \
     pos_cnum : %d; }@]"
    pos_fname
    pos_lnum
    pos_bol
    pos_cnum


(* Pretty-print a lexing buffer *)
let pp_print_lexbuf ppf     
  { Lexing.lex_buffer; 
    Lexing.lex_buffer_len; 
    Lexing.lex_abs_pos; 
    Lexing.lex_start_pos; 
    Lexing.lex_curr_pos; 
    Lexing.lex_last_pos;
    Lexing.lex_last_action;
    Lexing.lex_eof_reached;
    Lexing.lex_mem;
    Lexing.lex_start_p;
    Lexing.lex_curr_p } =

  Format.fprintf ppf 
    "@[<hv 2>{ lex_buffer : %s;@ \
     lex_buffer_len : %d;@ \
     lex_abs_pos : %d;@ \
     lex_start_pos : %d;@ \
     lex_curr_pos : %d;@ \
     lex_last_pos : %d;@ \
     lex_last_action : %d;@ \
     lex_eof_reached : %B;@ \
     lex_mem : %a;@ \
     lex_start_p : %a;@ \
     lex_curr_p : %a;@]"
    (Bytes.to_string lex_buffer)
    lex_buffer_len
    lex_abs_pos
    lex_start_pos
    lex_curr_pos
    lex_last_pos
    lex_last_action
    lex_eof_reached
    (pp_print_int_array 0) lex_mem
    pp_print_position lex_start_p
    pp_print_position lex_curr_p


  let char_for_backslash = function
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 'b' -> '\008'
    | 't' -> '\009'
    | c -> c

  (* Buffer to store strings *)
  let string_buf = Buffer.create 1024

  (* Buffer to store strings *)
  let string_buf = Buffer.create 1024

(* A stack of pairs of channels, a directory and lexing buffers to
   handle included files 

   The channel at the head of the list is the current channel to read
   from, the directory is the directory the channel is in, the lexing 
   buffer is the one to return to once all characters have been read 
   from the channel.

   Have only one lexing buffer and push a shallow copy of it to this
   stack when switching to an included file. At the end of the
   included file, restore the state of the lexing buffer from its
   shallow copy.

   When an eof is read from the lexing buffer, do not terminate but
   call pop_channel_of_lexbuf continue. If this raises the exception
   End_of_file, all input files have been read.
   
*)
let lexbuf_stack = ref []


(* Initialize the stack *)
let lexbuf_init channel curdir = 

  (* Reset input files before lexing. *)
  Flags.clear_input_files () ;

  (* A dummy lexing buffer to return to *)
  let lexbuf = Lexing.from_channel channel in

  (* Initialize the stack *)
  lexbuf_stack := [(channel, curdir, lexbuf)]


(* Switch to a new channel *)
let lexbuf_switch_to_channel lexbuf channel curdir = 

  (* Add channel and shallow copy of the previous lexing buffer to the
     top of the stack *)
  lexbuf_stack := 
    (channel, 
     curdir, 
     { lexbuf with 
         Lexing.lex_buffer = Bytes.copy lexbuf.Lexing.lex_buffer}) :: 
      !lexbuf_stack;
  
  (* Flush lexing buffer *)
  lexbuf.Lexing.lex_curr_pos <- 0;
  lexbuf.Lexing.lex_abs_pos <- 0;
  lexbuf.Lexing.lex_curr_p <- 
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_cnum = 0 };
  lexbuf.Lexing.lex_buffer_len <- 0


(* Pop lexing buffer from the stack an restore state of the lexing buffer *)
let pop_channel_of_lexbuf lexbuf =

  match !lexbuf_stack with 

    (* Exception if last channel has been popped off the stack *)
    | [] -> raise End_of_file

    (* Take channel and lexing buffer from top of stack *)
    | (ch, _, prev_lexbuf) :: tl -> 


      (* Close channel *)
      close_in ch; 

      (* Pop off stack *)
      lexbuf_stack := tl; 

      (* Restore state of the lexing buffer *)
      lexbuf.Lexing.lex_curr_pos <- prev_lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_abs_pos <- prev_lexbuf.Lexing.lex_abs_pos;
      lexbuf.Lexing.lex_curr_p <- prev_lexbuf.Lexing.lex_curr_p;
      lexbuf.Lexing.lex_buffer <- prev_lexbuf.Lexing.lex_buffer;
      lexbuf.Lexing.lex_buffer_len <- prev_lexbuf.Lexing.lex_buffer_len


(* Get the directory associated with the current channel *)
let curdir_of_lexbuf_stack () = match !lexbuf_stack with 
  | [] -> Sys.getcwd ()
  | (_, curdir, _) :: _ -> curdir



(* Function to read from the channel at the top of the stack *)
let read_from_lexbuf_stack buf n = 

  match !lexbuf_stack with 
    | [] -> 0
    | (ch, _, _) :: _ -> input ch buf 0 n

}


(* Define helper regexes *)
let ws = ['\t' ' ' '\009' '\012']+
(*let dos_newline = "\013\010"*)
let newline = '\r' | '\n' | "\r\n"
(*let newline = ('\010' | '\013' | "\013\010")*)

let comment = ";"
(*
let digit = ['0'-'9']
let integer = ['-' '+']? digit+
*)

let nat = ['1'-'9']
let digit = ('-')? ['0'-'9']

let hexdigit = ['0'-'9'] | ['a'-'f' 'A'-'F']
let hex = (hexdigit)+

let bin = ('0'|'1')+

let alpha = ['a'-'z' 'A'-'Z' '.' '$' '-' ':' '_' '\\']
let ident = (alpha)(alpha|digit)* (* regex for identifier *)

rule token = parse
  | comment   { skip_comment lexbuf}
  | ws    { token lexbuf }  
  | newline { NL }
  | digit+ as n  { Num (n) }

  (* Constants       

  | (bin as b)  { CONSTBIN (b)}
  | (dec as h)  {CONSTDEC (h)} 
  *)

  | "const" {CONSTBIN}        (* Bitvector Constant *)
  | "constd" {CONSTDEC}      (* Bitvector Constant Decimal *)   
  | "consth" {CONSTHEX}      (* Bitvector Constant Hex *) 

(*  | (hex as h)  { print_string h; } *) 

  | "sort" { SORT }        (* Sort Type *) 
  | "bitvec" {BITVEC}    (* BitVector / Register *)
  | "array" {ARRAY}      (* Array / Memory *)

  | "state" {STATE}
  | "input" {INPUT}    
  | "init" {INIT}        (* Initialization *)
  | "next" {NEXT}        (* Successor *)

  | "output" {OUTPUT}

  (* Properties *)
  | "bad" {BAD}
  | "constraint" {CONSTRAINT}
(*| "fair" {FAIR}
  | "Output" {OUTPUT}
  | "Justice" {JUSTICE} *)

  | "one" {ONE}       (* ? *)
  | "ones" {ONES}     (* Bitvector bv1 *)
  | "zero" {ZERO}     (* Bitvector bv0 *)

  (* ------------------------- *)
  (* [opidx] Indexed operators *)
  (* ------------------------- *)
  | "uext" {UEXT}      (* Unsigned extension *)
  | "sext" {SEXT}      (* signed extension *)    
  | "slice" {SLICE}   (* extraction *)   

  (* -------------------------------- *)
  (* [op] Unary unindexed operators  *)
  (* -------------------------------- *)
  | "not" { NOT }       (* Bit-wise *)
  | "neg" { NEG }       (* arithmetic *)

  | "redand" {REDAND}  (* reduction *)
  | "redor" {REDOR}    (* reduction *)
  | "redxor" {REDXOR}  (* reduction *)

  (* -------------------------------- *)
  (* Binary unindexed operators / op  *)
  (* -------------------------------- *)
  | "iff" {IFF}           (* Boolean *)
  | "implies" {IMPLIES}   (* Boolean *)

  | "eq" {EQ}             (* Equality *)
  | "neq" {NEQ}           (* Dis-equality *)

  (* Unsigned/Signed inequality *)
  | "ugt" {UGT}
  | "sgt" {SGT}
  | "ugte" {UGTE}
  | "sgte" {SGTE}
  | "ult" {ULT}
  | "slt" {SLT}
  | "ulte" {ULTE}
  | "slte" {SLTE}

  (* Boolean *)
  | "and" {AND}
  | "nand" {NAND}
  | "nor" {NOR}
  | "or" {OR} 
  | "xnor" {XNOR}
  | "xor" {XOR}

  (* Rotate, Shift *)
  | "rol" {ROL}
  | "ror" {ROR}
  | "sll" {SLL}
  | "sra" {SRA}
  | "srl" {SRL}

  (* Arithmetic *)
  | "add" {ADD}
  | "mul" {MUL}
  | "udiv" {UDIV}
  | "sdiv" {SDIV}
  | "smod" {SMOD}
  | "urem" {UREM}
  | "srem" {SREM}
  | "sub" {SUB}

  (* Overflow *)
  | "uaddo" {UADDO}
  | "saddo" {SADDO}
  | "udivo" {UDIVO}
  | "sdivo" {SDIVO}
  | "umulo" {UMULO}
  | "smulo" {SMULO}
  | "usubo" {USUBO}
  | "ssubo" {SSUBO}

  (* Concatenation *)
  |"concat" {CONCAT}

  (* Array Read *)
  | "read" {READ}

  (* -------------------------- *)
  (* (opidx) Ternary Operators *)
  (* -------------------------- *)
  | "ite" {ITE}      (* Conditional *)
  | "write" {WRITE}  (* Array Write *)
 (*print_string ("READ ID:" ^s ^ "\n");*)
  | ident as s { ID s }  
  | eof        { EOF }
  | _ { raise (Lexer_error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

 and skip_comment = parse
    newline    { next_line lexbuf; token lexbuf }
  | eof        { EOF }
  | _          { skip_comment lexbuf }
(* 
  
BTOR2 Lexer 

| SID (ARRAY or BITVEC)
| NID (INPUT or STATE)
| NID opidx SID NID UNIT
| NID op SID NID
| NID (INIT or NEXT) SID NID NID
| NID (BAD or CONSTRAINT or OUTPUT) NID {};

opidx:
| NID op SID NID

op:
| NID SID SID

special_char_replacements = {'$': '', '\\': '.', ':': '_c_'}

NL = '\n'

SN='N%s'

COM=';'

 *)