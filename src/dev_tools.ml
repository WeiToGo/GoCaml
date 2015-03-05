open Lexing
open Parser
open Ast
open Typecheck
open PrettyPrint

let get_ast input = 
  let ic = match input with
  | "stdin" -> stdin
  | filename -> open_in filename
  in
  let lexbuf = Lexing.from_channel ic in
  let ast = 
    try
      Parser.program Scan.wrapped_scan lexbuf;
    with Parser.Error
      -> (
          Printf.eprintf "%s" ("Syntax Error at line " 
            ^ (string_of_int lexbuf.lex_curr_p.pos_lnum)
            ^ ", column " 
            ^ (string_of_int (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol))
            ^ "-"
            ^ (string_of_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))
            ^ "\n"); raise Parser.Error
        )
    
  in
  match input with
  | "stdin" -> ast
  | filename -> close_in ic; ast

let print_lexer_stream input = 
  let ic = match input with
  | "stdin" -> stdin
  | filename -> open_in filename
  in
  (* let out_channel = open_out "lexer_stream.out" in  *)
  let _ = Printer.loop_token_printer Scan.wrapped_scan (Lexing.from_channel ic ) stdout
  in
  (* let () = close_out out_channel in *)
  match input with
  | "stdin" -> ()
  | filename -> close_in ic
