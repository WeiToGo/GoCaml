open Scan
open Parser
open Lexing
;;

let build_ast input = 
	let ic = match input with
	| "stdin" -> stdin
	| filename -> (try 
        open_in filename
      with Sys_error(s) -> Printf.fprintf stderr "Error: Failed to open %s\n" filename;raise (Sys_error(s)) )
	in
	let lexbuf = Lexing.from_channel ic in
	let ast = 
		try
			let ast = Parser.program Scan.wrapped_scan lexbuf in
			Weeder.weed_ast ast stderr; ast
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
    | Scan.Error s
    -> (
          Printf.eprintf "%s" ("Syntax Error at line " 
            ^ (string_of_int lexbuf.lex_curr_p.pos_lnum)
            ^ ", column " 
            ^ (string_of_int (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol))
            ^ "-"
            ^ (string_of_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))
            ^ "\n"); raise (Scan.Error s)
        )
  in
  (match input with
  | "stdin" -> ()
  | filename -> close_in ic); ast


let _ = if (Array.length Sys.argv) < 2 then
          print_string "You must supply the filename as first argument"
        else ()

let pptype = Sys.argv.(1)
let symtab = Sys.argv.(2)
let in_file_name = Sys.argv.(3);;

let out_directory = if (Array.length Sys.argv) > 4 then Sys.argv.(4) else (Sys.getcwd ()) ;;

let file_basename = String.sub in_file_name 0 ((String.length in_file_name) - 3) 

let pretty_file_name = file_basename ^ ".pretty.go"
let symtab_file_name = file_basename ^ ".symtab" 
let jasmin_file_name = file_basename ^ ".j"
let jasmin_class_name = file_basename
;;
let sym_out = open_out symtab_file_name

let () = Symtable.out_channel := sym_out

let inp = try open_in in_file_name 
  with Sys_error _ as e -> print_endline ("Error: Failed to open file " ^ in_file_name); raise e ;;
let filebuf = Lexing.from_channel inp 
let ast = build_ast in_file_name
;;

let () = if symtab = "t" then 
  Symtable.dumpsymtab := true 
  else Symtable.dumpsymtab := false ;;

let () = Typecheck.build_symbol_table ast;;

(* let () = if pptype = "t" then
  print_ast ast pretty_file_name 0
;; *)

(* let () = if pptype = "t" then
  print_ast ast jasmin_file_name jasmin_class_name
;; *)

let () = close_out sym_out ;; 

let out_directory = Filename.dirname in_file_name;;

let struct_map = StructCrawler.generate_struct_to_class_map ast in 
let () = CodeGen.scmap := (StructCrawler.struct_class_getter_factory_factory_JAVA struct_map) in 
let gostruct_list = StructCrawler.get_all_sructs struct_map in 
let struct_class_list = List.map StructCodeGen.create_struct_class gostruct_list in 
let () = List.iter (fun c -> CodeEmitter.print_struct_class c out_directory) struct_class_list in 
let bytecode_ast = CodeGen.create_byte_code_ast ast in_file_name in
CodeEmitter.print_main_class bytecode_ast (Filename.dirname in_file_name);;

let runtime_support_file_source = Filename.concat (Filename.dirname Sys.argv.(0)) "_build/staticlib/runtimesupport.j" in 
let runtime_support_file_dest = Filename.concat (Filename.dirname in_file_name) "runtimesupport.j" in 
Utils.copy_file runtime_support_file_source runtime_support_file_dest;;




