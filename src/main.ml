open Scan
open Parser
open PrettyPrint
open Lexing
;;

let build_ast input = 
	let ic = match input with
	| "stdin" -> stdin
	| filename -> open_in filename
	in
	let lexbuf = Lexing.from_channel ic in
	let () = 
		try
			let ast = Parser.program Scan.wrapped_scan lexbuf in
			(* print_string "VALID \n"; *)
			PrettyPrint.print_ast ast "out.go" 0;
			Weeder.weed_ast ast stderr;
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
  | "stdin" -> ()
  | filename -> close_in ic


let _ = if (Array.length Sys.argv) < 2 then
          print_string "You must supply the filename as first argument"
        else ()

let in_file_name = Sys.argv.(1)


let file_basename = String.sub in_file_name 0 ((String.length in_file_name) - 3) 

let pretty_file_name = file_basename ^ ".pretty.go"

let inp = open_in in_file_name 
let filebuf = Lexing.from_channel inp 
let ast = build_ast in_file_name
;;



(* let out_channel = open_out "lexer_stream.out" in 
let _ = Printer.loop_token_printer Scan.wrapped_scan (Lexing.from_channel stdin) out_channel
in
close_out out_channel
;;
 *)


(* pretty ast *)
