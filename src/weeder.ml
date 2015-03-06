open Ast
open Printf

(* Weed the ast *)
let weed_ast prog outchann =
	(* Helper print function*)
	let println s =
		fprintf outchann "%s\n" s
	in
	let loops = ref 0 in (* Nested loop level *)
	let shortvardcl = ref 1 in (* is shortvardcl allowed? *)
	let blankid = ref 0 in (* Is there a blankid currently? *)
	let rec visit_program prog =
		let Program(pack, decls) = prog in
		visit_package_decl pack;
		List.iter (fun x -> visit_lined_top_dcl x) decls
	and visit_package_decl pack =
		let Package(_, _) = pack in
		()
	and visit_lined_top_dcl ldl = 
		let LinedTD(dcl, _) = ldl in
		visit_top_dcl dcl
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
		| ID(str, _) -> ()
		| BlankID -> blankid := 1
	and visit_function_signature fs =
		let FunctionSig(args, top) = fs in
		List.iter visit_function_argument args;
		match top with
		| None -> ()
		| Some(ts) -> visit_type_spec ts
	and visit_function_argument arg =
		let FunctionArg(id, ts) = arg in
		visit_id id;
		visit_type_spec ts
	and visit_expression e =
		match e with
		| IdExp(id) -> visit_id id
		| LiteralExp(lit) -> visit_literal lit
		| UnaryExp(uoper, e) -> visit_expression e (* ignore unary op *)
		| BinaryExp(boper, e1, e2) -> (* ignore binary op *)
			visit_expression e1;
			visit_expression e2
		| FunctionCallExp(e, es) ->
			visit_expression e;
			List.iter visit_expression es
		| AppendExp(id, e) ->
			visit_id id;
			visit_expression e
		| TypeCastExp(ts, e) ->
			visit_type_spec ts;
			visit_expression e
		| IndexExp(e1, e2) ->
			visit_expression e1;
			visit_expression e2
		| SelectExp(e, id) ->
			visit_expression e;
			visit_id id
	and visit_literal lit =
		match lit with
		| IntLit(il) -> visit_int_literal il
		| FloatLit(str) -> ()
		| RuneLit(str) -> ()
		| StringLit(str) -> ()
		| RawStringLit(str) -> ()
	and visit_int_literal lit =
		match lit with
		| DecInt(str) -> ()
		| HexInt(str) -> ()
		| OctalInt(str) -> ()
	and visit_statement stmt =
		let LinedStatement(linenum, pstmt) = stmt in
		visit_plain_stmt pstmt linenum
	and visit_plain_stmt stmt linenum =
		match stmt with
		| EmptyStatement -> ()
		| ExpressionStatement(e) -> visit_expression e
		| AssignmentStatement(epairs) ->
			List.iter (fun (e1, e2) ->
				visit_expression e1;
				blankid := 0;
				visit_expression e2;
				(match !blankid with
				| 0 -> ()
				| _ ->
					println ("Cannot use _ as a value. Line: " ^ (string_of_int linenum));
					exit 0);
				blankid := 0) epairs
		| TypeDeclBlockStatement(tdcls) -> List.iter visit_type_dcl tdcls
		| ShortVarDeclStatement(svdcls) ->
			(match !shortvardcl with
			| 0 -> println ("Cannot declare in the for increment. Line: " ^ (string_of_int linenum))
			| _ -> List.iter visit_short_var_dcl svdcls)
		| VarDeclBlockStatement(mvdcls) -> List.iter visit_mul_var_dcl mvdcls
		| PrintStatement(es) -> List.iter visit_expression es
		| PrintlnStatement(es) -> List.iter visit_expression es
		| IfStatement(stmtop, e, stmts, stmtsop) ->
			(match stmtop with
			| None -> ()
			| Some(stmt) -> visit_statement stmt);
			visit_expression e;
			List.iter visit_statement stmts;
			(match stmtsop with
			| None -> ()
			| Some(stmtlist) -> List.iter visit_statement stmtlist)
		| ReturnStatement(eop) ->
			(match eop with
			| None -> ()
			| Some(e) -> visit_expression e)
		| SwitchStatement(stmtop, e, scs) ->
			let defcount = ref 0 in
			(match stmtop with
			| None -> ()
			| Some(stmt) -> visit_statement stmt);
			visit_expression e;
			List.iter (fun x -> visit_switch_case x defcount) scs;
			(match !defcount with
			| x when x > 1 ->
				println ("Multiple defaults in switch. Line: " ^ (string_of_int linenum));
				exit 0
			| _ -> ())
		| ForStatement(stmtop1, e, stmtop2, stmts) -> (* init, cond, post, body *)
			loops := !loops + 1;
			(match stmtop1 with
			| None -> ()
			| Some(stmt1) -> visit_statement stmt1);
			visit_expression e;
			shortvardcl := 0; (* disallow shortvardcl *)
			(match stmtop2 with
			| None -> ()
			| Some(stmt2) -> visit_statement stmt2);
			shortvardcl := 1; (* allow again *)
			List.iter visit_statement stmts;
			loops := !loops - 1
		| BreakStatement ->
			(match !loops with
			| 0 ->
				println ("Break statement is not in a loop. Line: " ^ (string_of_int linenum));
				exit 0
			| _ -> ())
		| ContinueStatement ->
			(match !loops with
			| 0 ->
				println ("Continue statement is not in a loop. Line: " ^ (string_of_int linenum));
				exit 0
			| _ -> ())
		| BlockStatement(stmts) -> List.iter visit_statement stmts
	and visit_switch_case sc defcount =
		match sc with
		| SwitchCase(es, stmts) ->
			List.iter visit_expression es;
			List.iter visit_statement stmts
		| DefaultCase(stmts) ->
			defcount := !defcount + 1;
			List.iter visit_statement stmts
	in
	visit_program prog
   

