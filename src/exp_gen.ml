open Ast

let lc = ref 0

let print_ast prog pretty = 
(* 	let Program(pack,ldl) = prog in *)
	let Program(exp) = prog in
	let outfile = open_out pretty in 
	let print_string = fun s -> output_string outfile s in
	let print_char = fun c -> output_char outfile c in
	let print_binop (binop, typ) = match typ with
		| GoInt ->
			let print_binop_int op = match op with
				| BinEq -> 
					print_string 
					"if_icmpeq Label_" ^ lc
					 "iconst_0"
					 "Label_" ^ lc ^ ":"
					 "iconst_1\n";
					 lc := !lc + 1
				| BinNotEq -> print_string 
					"if_icmpne Label_" ^ lc
					 "iconst_0"
					 "Label_" ^ lc ^ ":"
					 "iconst_1";
					 lc := !lc + 1;
				| BinLess -> print_string 
					"if_icmplt Label_" ^ lc
					 "iconst_0"
					 "Label_" ^ lc ^ ":"
					 "iconst_1\n";
					 lc := !lc + 1;
				| BinLessEq -> print_string
					"if_icmple Label_" ^ lc
					 "iconst_0"
					 "Label_" ^ lc ^ ":"
					 "iconst_1\n"
					 lc := !lc + 1;
				| BinGreater -> print_string
					"if_icmpgt Label_" ^ lc
					 "iconst_0"
					 "Label_" ^ lc ^ ":"
					 "iconst_1\n"
					 lc := !lc + 1;
				| BinGreaterEq -> print_string 
					"if_icmpge Label_" ^ lc
					 "iconst_0"
					 "Label_" ^ lc ^ ":"
					 "iconst_1\n"
					 lc := !lc + 1;
				| BinPlus -> print_string "iadd"
				| BinMinus -> print_string "isub"
				| BinBitOr -> print_string  "ior"
				| BinBitXor -> print_string "ixor"
				| BinMult -> print_string "imul"
				| BinDiv -> print_string "idiv"
				| BinMod -> print_string "irem"
				| BinShiftLeft -> print_string "ishl"
				| BinShiftRight -> print_string "ishr"
				| BinBitAnd -> print_string "iand"
				| BinBitAndNot -> print_string " &^ "
			in 
			print_binop_int binop
		| GoFloat ->
			let print_binop_fl op = match op with
				| BinEq -> print_string 
					"fcmpg"
					"ifeq Label_" ^ lc
					 "iconst_0"
					 "Label_" ^ lc ^ ":"
					 "iconst_1\n";
					 lc := !lc + 1;
				| BinNotEq -> print_string 
					"fcmpg"
					"ifne Label_" ^ lc
					 "iconst_0"
					 "Label_" ^ lc ^ ":"
					 "iconst_1\n";
					 lc := !lc + 1;
				| BinLess -> print_string 
					"fcmpg
					 iconst_m1
					 if_icmpeq Label_" ^ lc
					 "iconst_0"
					 "Label_" ^ lc ^ ":"
					 "iconst_1\n";
					 lc := !lc + 1;
				| BinLessEq -> print_string 
					" <= "
				| BinGreater -> print_string 
					"fcmpg
					 iconst_1
					 if_icmpeq Label_" ^ lc
					 "iconst_0
					 Label_" ^ lc ^ ":
					 iconst_1\n";
					 lc := !lc + 1;
				| BinGreaterEq -> print_string " >= "
				| BinPlus -> print_string " fadd "
				| BinMinus -> print_string " fsub "
				| BinMult -> print_string " fmul "
				| BinDiv -> print_string " fdiv "
				| BinMod -> print_string " frem "
			in 
			print_binop_fl binop
		| GoBool ->
			let print_binop_bool op = match op with
				| BinOr -> print_string 
					"ifne Label_" ^ lc
					"ifne Label_" ^ lc
					"iconst_1"
					"Label_" ^ lc ^ ":"
					 "iconst_0";
					 lc := !lc + 1;
				| BinAnd -> print_string 
					"ifeq Label_" ^ lc
					"ifeq Label_" ^ lc
					"iconst_1"
					"Label_" ^ lc ^ ":"
					 "iconst_0";
					 lc := !lc + 1;
			in 
			print_binop_bool binop
	in
	(* Assume the result of the exp following the unary op is already on stack*)
	let print_unary_op (unop, typ) = match typ with
		| GoInt ->
			let print_uop_int op = match op with
				| UPlus -> ()
				| UMinus -> print_string "ineg "
				| UCaret -> print_string " ^ "
			in print_uop_int unop 
		| GoFloat ->
			let print_unop_fl op = match op with
				| UPlus -> ()
				| UMinus -> print_string "fneg "
			in print_unop_fl unop
		| GoRune ->
			let print_unop_rune op = match op with
				| UPlus -> ()
				| UMinus -> print_string " ineg "
				| UCaret -> print_string " ^ "
			in print_unop_rune unop
		| GoBool -> 
			let print_unop_bool op = match op with
				| UNot -> print_string 
					"ifeq Label 1\niconst_0\n"		
			in print_unop_bool unop
	in
	let print_identifier id = match id with
		| ID (s, _) -> print_string s 
		| BlankID -> print_string "_ "
	in
	(* Leave the result of the expression on top of the stack. *)
	let rec print_expr (Expression(exp, typ)) = 
	let () = (match exp with
	| IdExp (i) -> print_identifier i typ
	| LiteralExp (l) -> print_literal l typ
	| UnaryExp (op, e, t) -> 
		begin
			print_expr e;
			print_unary_op (op, t);
		end
	| BinaryExp (op, e1, e2, t) ->
		begin
			print_expr e1;
			print_expr e2; 
			print_binop (op, t);
		end)
	in 
	let print_int_literal lit = match lit with
		| Ast.DecInt (s) -> ()
		| Ast.HexInt (s) -> ()
		| Ast.OctalInt (s) -> ()
	in
	let print_literal lit = match lit with
		| IntLit (i) -> print_string "iconst_" ^ i ^ "\n"
		| FloatLit (f) -> print_string "ldc " ^ f ^ "\n"
		| RuneLit (r) -> ()
		| StringLit (s) -> print_string "ldc " ^ f ^ "\n"
		| RawStringLit (s) -> ()
	in
	let rec print_multi_struct_field level msf= match msf with
		| MultipleStructFieldDecl (ssf_list) -> ()
	and print_type_spec level ts = match ts with
		| BasicType (bt) -> ()
		| SliceType (t) -> ()
		| ArrayType (int_lit, t) -> ()
		| StructType (st) -> ()
		| FunctionType (tl, ts_op) ->
			begin
				print_string " func ( ";
				List.iter (fun x -> print_type_spec level x) tl;
				print_string ") ";
				match ts_op with
				| None -> ()
				| Some ts_op ->  print_type_spec level ts_op
			end
		| CustomType (id) -> ()
	in
	let print_func_arg level fa = match fa with
		| FunctionArg (id,t) ->
			begin
				print_identifier id;
				print_type_spec level t;
			end
	in
	let print_func_sign level fs = match fs with
		| FunctionSig (f_list, t_op) -> ()
	in
	let print_multi_var_decl level mvd = match mvd with
	| MultipleVarDecl (svd_list) -> ()
	in
	let print_var_decl level mvd_list = ()
	in
	let print_short_var_decl level svd_list  = ()
	in
	let rec print_stmt level stmt = 
		match stmt with
		| EmptyStatement -> ()
		| ExpressionStatement (e) -> ()
		| AssignmentStatement (l)-> ()
		| TypeDeclBlockStatement (decl_list)-> ()
		| VarDeclBlockStatement (decl_list)-> ()
		| ShortVarDeclStatement(decl_list)-> ()
		| PrintStatement (e_list)-> ()
		| PrintlnStatement (e_list)-> ()
		| IfStatement (s, e, s1_list, s2_list) -> ()
		| ReturnStatement (e_op)-> ()
		| SwitchStatement (s_op,e,case_list)-> ()
		| ForStatement (s1_op,e_op,s2_op,stmt_list)-> ()

		| BreakStatement -> ()
		| ContinueStatement  -> ()
		| BlockStatement (sl) -> ()
	and print_stmt_wrap level stmt = match stmt with
		| LinedStatement (ln,s) -> print_stmt level s
	and print_statement_list level st_list = match st_list with
				| h :: t -> 
					begin
						print_stmt_wrap (level + 1) h;
 						print_string ";\n";
 						print_statement_list level t;
 					end
				| [] -> ()
	in
	let print_top_decl level td = match td with 
		| FunctionDecl (id,fs, stmt_list) ->
			begin
				print_string "func ";
				print_identifier id;
				print_func_sign level fs;
				print_string " { \n";
				insert_tab(level);
				print_statement_list level stmt_list;
				insert_tab(level);
				print_string "}";
			end
		| TypeDeclBlock (tl) -> print_type_decl level tl
		| VarDeclBlock (vl) -> print_var_decl level vl
	in
	let print_lined_top_decl_list level ldl = 
		List.iter 
			(fun x -> 
				let LinedTD(td, _) = x in 
				let () = print_top_decl level td in
				print_string ";\n") ldl
	in
	print_expr exp;
	close_out outfile
