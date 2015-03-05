open Ast
open Printf

(* Weed the ast *)
let weed_ast prog outchann =
	(* Helper print function*)
	let println s =
		fprintf outchann "%s\n" s
	in
	let rec visit_program prog =
		let Program(pack, decls) = prog in
		visit_package_decl pack;
		List.iter (fun x -> visit_top_dcl x) decls
	and visit_package_decl pack =
		()
	and visit_top_dcl dcl =
		match dcl with
		| FunctionDecl (id,fs, stmts) ->
			visit_id id;
			visit_function_signature fs;
			List.iter visit_statement stmts
		| TypeDeclBlock (tds) -> List.iter visit_type_dcl tds
		| VarDeclBlock (mvdcls) -> List.iter visit_mul_var_dcl mvdcls
	and visit_mul_var_dcl dcl =
		let MultipleVarDecl(svdcls) = dcl in
		List.iter visit_sin_var_dcl svdcls
	and visit_sin_var_dcl dcl =
		let SingleVarDecl(id, top, eop) = dcl in
		visit_id id;
		match top with
		| None -> ()
		| Some(t) -> visit_type_spec t;
		match eop with
		| None -> ()
		| Some(e) -> visit_expression e
	and visit_short_var_dcl dcl =
		let ShortVarDecl(id, e) = dcl in
		visit_id id;
		visit_expression e
	and visit_type_dcl dcl =
		let SingleTypeDecl(id, ts) = dcl in
		visit_id id;
		visit_type_spec ts
	and visit_type_spec ts =
		match ts with
		| BasicType(bt) -> visit_basic_type bt
		| SliceType(tp) -> visit_type_spec tp
		| ArrayType(il, tp) ->
			visit_int_literal il;
			visit_type_spec tp
		| StructType(msdcls) -> List.iter visit_mul_str_dcl msdcls
		| FunctionType(tps, top) ->
			List.iter visit_type_spec tps;
			(match top with
			| None -> ()
			| Some(t) -> visit_type_spec t)
		| CustomType(id) -> visit_id id

	and visit_mul_str_dcl dcl =
		let MultipleStructFieldDecl(ssdcls) = dcl in
		List.iter visit_sin_str_dcl ssdcls
	and visit_sin_str_dcl dcl =
		let SingleStructFieldDecl(id, ts) = dcl in
		visit_id id;
		visit_type_spec ts
	and visit_basic_type bt =
		match bt with
		| IntType -> ()
		| FloatType -> ()
		| BoolType -> ()
		| RuneType -> ()
		| StringType -> ()
	and visit_id id =
		match id with
		| IdName(str) -> ()
		| BlankID -> ()
	and visit_statement stmt =
		println "stmt"
	and visit_function_signature fs =
		println "fs"
	and visit_expression e =
		println "expression"
	and visit_int_literal lit =
		println "intlit"
	in
	visit_program prog
   
