open Lexing
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
  | filename -> close_in ic; ast;;

let testfile = "../../tests/random_tests/helloworld.go";;

let goast = get_ast testfile;;

Typecheck.build_symbol_table goast;;

let bytecode_ast = CodeGen.create_byte_code_ast goast (Filename.basename testfile);;

CodeEmitter.print_main_class bytecode_ast (Filename.dirname testfile);;

