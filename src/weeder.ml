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
	let lvalue = ref 0 in (* Was the last expression a left value? *)
	let checkForBlankReadError linenum =
		match !blankid with
		| 0 -> ()
		| _ ->
			println ("Cannot use _ as a value. Line: " ^ (string_of_int linenum));
			exit 1
	in
	let checkForLeftValueError linenum =
		match !lvalue with
		| 0 ->
			println ("Cannot assign to expression. Line: " ^ (string_of_int linenum));
			exit 1
		| _ -> ()
	in
	let rec visit_program prog =
		let Program(pack, decls) = prog in
		visit_package_decl pack;
		List.iter (fun x -> visit_lined_top_dcl x) decls
	and visit_package_decl pack =
		let Package(_, _) = pack in
		()
	and visit_lined_top_dcl ldl = 
		let LinedTD(dcl, linenum) = ldl in
		visit_top_dcl dcl linenum
	and visit_top_dcl dcl linenum =
		match dcl with
		| FunctionDecl (id,fs, stmts) ->
			visit_id id;
			visit_function_signature fs linenum;
			List.iter visit_statement stmts
		| TypeDeclBlock (tds) -> List.iter (fun x -> visit_type_dcl x linenum) tds
		| VarDeclBlock (mvdcls) -> List.iter (fun x -> visit_mul_var_dcl x linenum) mvdcls
	and visit_mul_var_dcl dcl linenum =
		let MultipleVarDecl(svdcls) = dcl in
		List.iter (fun x -> visit_sin_var_dcl x linenum) svdcls
	and visit_sin_var_dcl dcl linenum =
		let SingleVarDecl(id, top, eop) = dcl in
		visit_id id;
		(match top with
		| None -> ()
		| Some(t) -> visit_type_spec t linenum);
		(match eop with
		| None -> ()
		| Some(e) ->
			blankid := 0;
			visit_expression e linenum;
			checkForBlankReadError linenum;
			blankid := 0);
	and visit_short_var_dcl dcl linenum =
		let ShortVarDecl(id, e) = dcl in
		visit_id id;
		blankid := 0;
		visit_expression e linenum;
		checkForBlankReadError linenum;
		blankid := 0
	and visit_type_dcl dcl linenum =
		let SingleTypeDecl(id, ts) = dcl in
		visit_id id;
		visit_type_spec ts linenum
	and visit_type_spec ts linenum =
		match ts with
		| BasicType(bt) -> visit_basic_type bt
		| SliceType(tp) -> visit_type_spec tp linenum
		| ArrayType(il, tp) ->
			visit_int_literal il;
			visit_type_spec tp linenum
		| StructType(msdcls) -> List.iter (fun x -> visit_mul_str_dcl x linenum) msdcls
		| FunctionType(tps, top) ->
			List.iter (fun x -> visit_type_spec x linenum) tps;
			(match top with
			| None -> ()
			| Some(t) -> visit_type_spec t linenum)
		| CustomType(id) ->
			blankid := 0;
			visit_id id;
			checkForBlankReadError linenum;
			blankid := 0
	and visit_mul_str_dcl dcl linenum =
		let MultipleStructFieldDecl(ssdcls) = dcl in
		List.iter (fun x -> visit_sin_str_dcl x linenum) ssdcls
	and visit_sin_str_dcl dcl linenum =
		let SingleStructFieldDecl(id, ts) = dcl in
		visit_id id;
		visit_type_spec ts linenum
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
	and visit_function_signature fs linenum =
		let FunctionSig(args, top) = fs in
		List.iter (fun x -> visit_function_argument x linenum) args;
		match top with
		| None -> ()
		| Some(ts) -> visit_type_spec ts linenum
	and visit_function_argument arg linenum =
		let FunctionArg(id, ts) = arg in
		visit_id id;
		visit_type_spec ts linenum
	and visit_expression e linenum =
		match e with
		| IdExp(id) ->
			visit_id id;
			lvalue := 1
		| LiteralExp(lit) ->
			visit_literal lit;
			lvalue := 0
		| UnaryExp(uoper, e) ->
			visit_expression e linenum; (* ignore unary op *)
			lvalue := 0 (* Unary op breaks an lvalue *)
		| BinaryExp(boper, e1, e2) -> (* ignore binary op *)
			visit_expression e1 linenum;
			visit_expression e2 linenum;
			lvalue := 0 (* Unary op breaks an lvalue *)
		| FunctionCallExp(e, es) ->
			visit_expression e linenum;
			blankid := 0;
			List.iter (fun x -> visit_expression x linenum) es;
			checkForBlankReadError linenum;
			blankid := 0;
			lvalue := 0 (* cannot assign to function calls *)
		| AppendExp(id, e) ->
			visit_id id;
			visit_expression e linenum;
			lvalue := 0 (* cannot assign to append *)
		| TypeCastExp(ts, e) ->
			visit_type_spec ts linenum;
			blankid := 0;
			visit_expression e linenum;
			checkForBlankReadError linenum;
			blankid := 0;
			lvalue := 0 (* cannot assign to casts *)
		| IndexExp(e1, e2) ->
			visit_expression e1 linenum;
			visit_expression e2 linenum;
			lvalue := 1
		| SelectExp(e, id) ->
			visit_expression e linenum;
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
		| ExpressionStatement(e) -> visit_expression e linenum
		| AssignmentStatement(epairs) ->
			List.iter (fun (e1, e2) ->
				visit_expression e1 linenum;
				checkForLeftValueError linenum;
				blankid := 0;
				visit_expression e2 linenum;
				checkForBlankReadError linenum;
				blankid := 0) epairs
		| TypeDeclBlockStatement(tdcls) -> List.iter (fun x -> visit_type_dcl x linenum) tdcls
		| ShortVarDeclStatement(svdcls) ->
			(match !shortvardcl with
			| 0 ->
				println ("Cannot declare in the for increment. Line: " ^ (string_of_int linenum));
				exit 1
			| _ -> List.iter (fun x -> visit_short_var_dcl x linenum) svdcls)
		| VarDeclBlockStatement(mvdcls) -> List.iter (fun x -> visit_mul_var_dcl x linenum) mvdcls
		| PrintStatement(es) -> List.iter (fun x -> visit_expression x linenum) es
		| PrintlnStatement(es) -> List.iter (fun x -> visit_expression x linenum) es
		| IfStatement(stmtop, e, stmts, stmtsop) ->
			(match stmtop with
			| None -> ()
			| Some(stmt) -> visit_statement stmt);
			visit_expression e linenum;
			List.iter visit_statement stmts;
			(match stmtsop with
			| None -> ()
			| Some(stmtlist) -> List.iter visit_statement stmtlist)
		| ReturnStatement(eop) ->
			(match eop with
			| None -> ()
			| Some(e) -> visit_expression e linenum)
		| SwitchStatement(stmtop, e, scs) ->
			let defcount = ref 0 in
			(match stmtop with
			| None -> ()
			| Some(stmt) -> visit_statement stmt);
			visit_expression e linenum;
			List.iter (fun x -> visit_switch_case x defcount linenum) scs;
			(match !defcount with
			| x when x > 1 ->
				println ("Multiple defaults in switch. Line: " ^ (string_of_int linenum));
				exit 1
			| _ -> ())
		| ForStatement(stmtop1, e, stmtop2, stmts) -> (* init, cond, post, body *)
			loops := !loops + 1;
			(match stmtop1 with
			| None -> ()
			| Some(stmt1) -> visit_statement stmt1);
			visit_expression e linenum;
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
				exit 1
			| _ -> ())
		| ContinueStatement ->
			(match !loops with
			| 0 ->
				println ("Continue statement is not in a loop. Line: " ^ (string_of_int linenum));
				exit 1
			| _ -> ())
		| BlockStatement(stmts) -> List.iter visit_statement stmts
	and visit_switch_case sc defcount linenum =
		match sc with
		| SwitchCase(es, stmts) ->
			List.iter (fun x -> visit_expression x linenum) es;
			List.iter visit_statement stmts
		| DefaultCase(stmts) ->
			defcount := !defcount + 1;
			List.iter visit_statement stmts
	in
	visit_program prog
   

