open Ast
open Printf

exception NotImplemented

(* Weed the ast *)
let weed_ast prog outchann =
	(* Helper print function*)
	let println s =
		fprintf outchann "%s\n" s
	in
	let visit_id id =
		()
	in
	let visit_function_signature fs =
		()
	in
	let visit_statement stmt =
		()
	in
	let visit_var_decl dcl =
		()
	in
	let visit_type_decl dcl =
		()
	in
	let visit_top_dcl dcl =
		match dcl with
		| FunctionDecl (id,fs, stmts) ->
			visit_id id;
			visit_function_signature fs;
			List.iter visit_statement stmts
		| TypeDeclBlock (tl) -> List.iter visit_type_decl tl
		| VarDeclBlock (vl) -> List.iter visit_var_decl vl
	in
	let visit_top_dcl_list decls =
		List.iter (fun x -> visit_top_dcl x) decls
	in
	let visit_package_decl pack =
		()
	in
	let visit_program prog =
		let Program(pack, decls) = prog in
		visit_package_decl pack;
		visit_top_dcl_list decls
	in
	visit_program prog
   
