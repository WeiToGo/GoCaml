open Ast
open Symtable

let lc = ref 0

let print_ast prog pretty = 
	let Program(pack,ldl) = prog in
	(* let Program(exp) = prog in *)
	let outfile = open_out pretty in 
	let print_string = fun s -> output_string outfile s in
	let print_char = fun c -> output_char outfile c in
	let print_binop (binop, typ) = match typ with
		| GoInt ->
			let print_binop_int op = match op with
				| BinEq -> 
					print_string
					("if_icmpeq Label_" ^ string_of_int !lc ^ "\n" ^
					"iconst_0
					 Label_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1					
				| BinNotEq -> print_string 	 
					("if_icmpne Label_" ^ string_of_int !lc ^ "\n" ^
					"iconst_0
					 Label_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1
				| BinLess -> print_string 
					("if_icmplt Label_" ^ string_of_int !lc ^ "\n" ^
					 "iconst_0
					 Label_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1
				| BinLessEq -> print_string 
					("if_icmple Label_" ^ string_of_int !lc ^ "\n" ^
					 "iconst_0
					 Label_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1
				| BinGreater -> print_string 
					("if_icmpgt Label_" ^ string_of_int !lc ^ "\n" ^
					 "iconst_0
					 Label_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1
				| BinGreaterEq -> print_string
					("if_icmpge Label_" ^ string_of_int !lc ^ "\n" ^
					 "iconst_0
					 Label_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1
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
				| BinEq -> 
					print_string 
					("fcmpg
					 ifeq Label_" ^ string_of_int !lc ^
					 "iconst_0
					  Label_" ^ string_of_int !lc ^ ":
					  iconst_1\n");
					 lc := !lc + 1
				| BinNotEq -> print_string 
					("fcmpg
					 ifne Label_" ^ string_of_int !lc ^
					 "iconst_0
					 Label_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1
				| BinLess -> print_string 
					("fcmpg
					 iconst_m1
					 if_icmpeq Label_" ^ string_of_int !lc ^
					 "iconst_0
					  Label_" ^ string_of_int !lc ^ ":
					  iconst_1\n");
					 lc := !lc + 1
				| BinLessEq -> print_string " <= "
				| BinGreater -> print_string
					("fcmpg
					 iconst_1
					 if_icmpeq Label_" ^ string_of_int !lc ^
					 "iconst_0
					 Label_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1
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
					("ifne Label_" ^ string_of_int !lc ^
					"ifne Label_" ^ string_of_int !lc ^
					"iconst_1
					 Label_" ^ string_of_int !lc ^ ":
					 iconst_0");
					 lc := !lc + 1
				| BinAnd -> print_string 
					("ifeq Label_" ^ string_of_int !lc ^
					"ifeq Label_" ^ string_of_int !lc ^
					"iconst_1
					 Label_" ^ string_of_int !lc ^ ":
					 iconst_0");
					 lc := !lc + 1
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
	(* load from different places depending if id is from func arg or a local var*)
	let print_identifier id typ = match id with
		| ID (s, _) -> ()
		| BlankID -> ()
	in
	let print_int_literal lit = match lit with
		| Ast.DecInt (s) -> print_string ("iconst_" ^ s ^ "\n")
		| Ast.HexInt (s) -> ()
		| Ast.OctalInt (s) -> ()
	in
	let print_literal lit = match lit with
		| IntLit (i) -> print_int_literal i
		| FloatLit (f) -> print_string ("ldc " ^ f ^ "\n")
		| RuneLit (r) -> ()
		| StringLit (s) -> print_string ("ldc " ^ s ^ "\n")
		| RawStringLit (s) -> ()
	in
	(* Leave the result of the expression on top of the stack. *)
	let rec print_expr (Expression(exp, typ)) = match !typ with
		| None -> ()
		| Some (t) -> match exp with
			| IdExp (i) -> print_identifier i t
			| LiteralExp (l) -> print_literal l
			| UnaryExp (op, Expression(e, tp)) -> 
				begin
					print_expr (Expression(e, typ));
					print_unary_op (op, t);
				end
			| BinaryExp (op, Expression(e1, t1), Expression(e2, t2)) ->
				begin
					print_expr (Expression(e1, t1));
					print_expr (Expression(e2, t2)); 
					print_binop (op, t);
				end		
	in
	let print_basic_type t = match t with
		| IntType -> print_string "I"
		| FloatType -> print_string "F"
		| BoolType -> ()
		| RuneType -> ()
		| StringType -> print_string "Ljava/lang/String;"
	in
	let rec print_multi_struct_field level msf= match msf with
		| MultipleStructFieldDecl (ssf_list) -> ()
	(* type_spec only used in func declarations *)
	and print_type_spec ts = match ts with
		| BasicType (bt) -> print_basic_type bt
		| SliceType (t) -> ()
		| ArrayType (int_lit, t) -> ()
		| StructType (st) -> ()
		| FunctionType (tl, ts_op) -> ()
		| CustomType (id) -> ()
	in
	let print_func_return ret_op = match ret_op with
		| None -> print_string "V"
		| Some (r) -> print_type_spec r
	in
	let print_func_arg fa = match fa with
		| FunctionArg (id,t) ->	print_type_spec t;
	in
	let print_func_sign fs = match fs with
		| FunctionSig (f_list, t_op) -> 
			begin
				print_string "(";
				List.iter print_func_arg f_list;
				print_string ")";
				print_func_return t_op;		
				print_string "\n"		
			end
	in
	let print_multi_var_decl mvd = match mvd with
	| MultipleVarDecl (svd_list) -> ()
	in
	let print_var_decl mvd_list = ()
	in
	let print_short_var_decl svd_list  = ()
	in
	let rec print_stmt stmt = 
		match stmt with
		| EmptyStatement -> ()
		| ExpressionStatement (e) -> print_expr e
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
	and print_stmt_wrap stmt = match stmt with
		| LinedStatement (ln,s) -> print_stmt s
	and print_statement_list st_list = match st_list with
				| h :: t -> 
					begin
						print_stmt_wrap h;
 						print_statement_list t;
 					end
				| [] -> ()
	in 
	let print_top_decl td = match td with 
		| FunctionDecl (id,fs, stmt_list) -> 
			(match id with 
			| ID (s, _) ->
				begin
					print_string (".method public static " ^ s);
					print_func_sign fs;
					print_string ".limit stack 100\n.limit locals 100\n";
					List.iter print_stmt_wrap stmt_list
				end)
		| TypeDeclBlock (tl) -> ()
		| VarDeclBlock (vl) -> ()
	in
	let print_lined_top_decl_list ldl = 
		List.iter 
			(fun x -> 
				let LinedTD(td, _) = x in 
				let () = print_top_decl td in
				print_string ";\n") ldl
	in
	print_lined_top_decl_list ldl;
	close_out outfile

