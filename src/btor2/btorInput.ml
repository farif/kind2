open Lib
open BtorReporting
open Lexing
open MenhirLib.General
open Format 

module LA = BtorAst

module LPMI = BtorParser.MenhirInterpreter
module LL = BtorLexer          
module LPE = BtorParserErrors
module LPI = BtorParser.Incremental
module TC = BtorTypeChecker
module I = BtorIdent

let (>>=) = Res.(>>=)
let (>>) = Res.(>>)
          
exception NoMainNode of string

(* The parser has succeeded and produced a semantic value.*)
let success (v : BtorAst.btor): BtorAst.btor =
(*  Log.log L_trace "Parsed :\n=========\n\n%a\n@." LA.pp_btor v;*)
  v

(* Generates the appropriate parser error message *)
let build_parse_error_msg env =
    match LPMI.stack env with
    | lazy Nil -> "Syntax Error"
    | lazy (Cons (LPMI.Element (state, _, _, _), _)) ->
       let pstate = LPMI.number state in
       let error_msg = try (LPE.message pstate) with
                       | Not_found -> "Syntax Error! " 
                                      ^ "Please report this issue with a minimum working example." in
       Log.log L_debug "(Parser Error State: %d)" pstate;
       error_msg
                                     

(* Raises the [Parser_error] exception with appropriate position and error message *)
let fail env lexbuf =
  let emsg = build_parse_error_msg env in
  let pos = position_of_lexing lexbuf.lex_curr_p in
  fail_at_position pos emsg

(* Incremental Parsing *)
let rec parse lexbuf (chkpnt : LA.btor LPMI.checkpoint) =
  match chkpnt with
  | LPMI.InputNeeded _ ->
     let token = LL.token lexbuf in
     let startp = lexbuf.lex_start_p
     and endp = lexbuf.lex_curr_p in
     let chkpnt = LPMI.offer chkpnt (token, startp, endp) in
     parse lexbuf chkpnt
  | LPMI.Shifting _
  | LPMI.AboutToReduce _ ->
     let chkpnt = LPMI.resume chkpnt in
     parse lexbuf chkpnt
  | LPMI.HandlingError env ->
     fail env lexbuf
  | LPMI.Accepted v -> success v
  | LPMI.Rejected ->
     fail_no_position "Parser Error: Parser rejected the input."
  

(* Parses input channel to generate an AST *)
let ast_of_channel(in_ch: in_channel): BtorAst.btor =

  let input_source = Flags.input_file () in
  (* Create lexing buffer *)
  let lexbuf = Lexing.from_function BtorLexer.read_from_lexbuf_stack in

  (* Initialize lexing buffer with channel *)
  BtorLexer.lexbuf_init 
    in_ch
    (try Filename.dirname (input_source)
     with Failure _ -> Sys.getcwd ());
  (* Set the relative file name in lex buffer *)
  (* Lib.set_lexer_filename lexbuf (input_source); *)
  (* Setting the name in the lexer buffer causes issues with the way we display properties.
   * For the time being we would not want to change this behaviour *)

  (* Create lexing buffer and incrementally parse it*)
  try
    (parse lexbuf (LPI.main lexbuf.lex_curr_p))
  with
  | BtorLexer.Lexer_error err -> fail_at_position (Lib.position_of_lexing lexbuf.lex_curr_p) err  

let func_report (results: TC.tc_type TC.tc_result)  = 
  match results with   
    | Ok d -> Log.log L_note "OK" ; TC.pp_tc_type std_formatter d  
    | Error err -> failwith "Type Checking Failed" 

let rec funct_list_report = function
  | [] -> Log.log L_note "Type checking done"
  | h :: t ->  func_report h ; funct_list_report t
  
(* Parse from input channel *)
let of_channel in_ch =
  (* Get declarations from channel. *)
  let btorprog = ast_of_channel in_ch
  in 
    if Flags.no_tc ()
     then btorprog
     else 
       let tc_res = TC.typ_check_btor_prog btorprog                   
          in
          Log.log L_note "Typechecking enabled.";
          funct_list_report tc_res;  btorprog 
          
(* Returns the AST from a file. *)
let ast_of_file filename =
  (* Open the given file for reading *)
  let in_ch = match filename with
    | "" -> stdin
    | _ -> open_in filename
  in
  ast_of_channel in_ch

  (* Open and parse from file *)
let of_file filename =
  (* Open the given file for reading *)
  let in_ch = match filename with
    | "" -> stdin
    | _ -> open_in filename
  in
  let btor_nodes = of_channel in_ch in
      let top = I.mk_string_ident "top" in 
        (* Scope of the system from node name *)
      let scope = [I.string_of_ident false top] in

      (* Does node have contracts? *)
      let has_contract = false in 

      (* Does node have modes? *)
      let has_modes = false in

      (* Does node have an implementation? *)
      let has_impl = false in

      (* Construct subsystem of node *)
      let subsystem = 

        { SubSystem.scope;
          SubSystem.source = btor_nodes;
          SubSystem.has_contract;
          SubSystem.has_modes;
          SubSystem.has_impl;
          SubSystem.subsystems = []  }

      in
          subsystem

