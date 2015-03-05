open Ast
open Printf
open Symtable

(* globals *)

let out_channel = stdout
let err_channel = stderr

exception NotImplemented
exception TypeCheckError of string




(* --~~~~~~--*** The Type Checker ***--~~~~~~-- *)


let rec tc_program ctx = function
  | Program(pkg_decl, top_lined_dc_list) ->
    begin
       tc_package_decl ctx pkg_decl;
       List.iter (tc_lined_top_decl ctx) top_lined_dc_list
    end 
and tc_package_decl ctx = function
  | Package(_) -> () (* Nothing to typecheck *)
and tc_lined_top_decl ctx = function
  | LinedTD(tdcl, ln) -> 
      try tc_top_decl ctx tdcl
      with (TypeCheckError s) -> 
        fprintf err_channel "%s" ("Typing Error at line " ^ (string_of_int ln) ^ ":\n");
        fprintf err_channel "%s" s
and tc_top_decl ctx = function
  | FunctionDecl(_, _, _) -> raise NotImplemented
  | TypeDeclBlock(_) -> raise NotImplemented
  | VarDeclBlock(m_var_decl_list) -> 
      List.iter (tc_multiple_var_declaration ctx) m_var_decl_list
and tc_multiple_var_declaration ctx = function
  | MultipleVarDecl(_) -> raise NotImplemented
and tc_single_var_declaration ctx node = raise NotImplemented
and tc_short_var_decl ctx node = raise NotImplemented
and tc_type_declaration ctx node = raise NotImplemented
and tc_type_spec ctx node = raise NotImplemented
and tc_multi_struct_field_decl ctx node = raise NotImplemented
and tc_single_struct_field_decl ctx node = raise NotImplemented
and tc_basic_type ctx node = raise NotImplemented
and tc_identifier ctx node = raise NotImplemented
and tc_function_signature ctx node = raise NotImplemented
and tc_function_arg ctx node = raise NotImplemented
and tc_expression ctx node = raise NotImplemented
and tc_literal ctx node = raise NotImplemented
and tc_int_literal ctx node = raise NotImplemented
and tc_unary_op ctx node = raise NotImplemented
and tc_binary_op ctx node = raise NotImplemented
and tc_statement ctx node = raise NotImplemented
and tc_plain_statement ctx node = raise NotImplemented
and tc_switch_case ctx node = raise NotImplemented

let build_symbol_table ast =
  let global_scope = initial_scope ()  (* 1024 is the initial hash table size *)
  in tc_program global_scope ast
