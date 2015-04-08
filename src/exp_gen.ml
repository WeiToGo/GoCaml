open Ast
open Symtable

let lc = ref 0

exception InternalError of string

let print_ast prog file class_name = 
	let Program(pack,ldl) = prog in
	(* let Program(exp) = prog in *)
	let outfile = open_out file in 
	let print_string = fun s -> output_string outfile s in
	let print_char = fun c -> output_char outfile c in
	(* for boolean results, true = 1, false = 0 *)
	let print_binop (binop, typ) = match typ with
		| GoInt -> (* TO DO need to fix label jumping*)
			let print_binop_int op = match op with
				| BinEq -> 
					print_string
					("if_icmpeq Label_" ^ string_of_int !lc ^ "\n" ^
					"iconst_0\nLabel_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1					
				| BinNotEq -> print_string 	 
					("if_icmpne Label_" ^ string_of_int !lc ^ "\n" ^
					"iconst_0\nLabel_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1
				| BinLess -> print_string 
					("if_icmplt Label_" ^ string_of_int !lc ^ "\n" ^
					 "iconst_0\nLabel_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1
				| BinLessEq -> print_string 
					("if_icmple Label_" ^ string_of_int !lc ^ "\n" ^
					 "iconst_0\nLabel_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1
				| BinGreater -> print_string 
					("if_icmpgt Label_" ^ string_of_int !lc ^ "\n" ^
					 "iconst_0\nLabel_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1
				| BinGreaterEq -> print_string
					("if_icmpge Label_" ^ string_of_int !lc ^ "\n" ^
					 "iconst_0\nLabel_" ^ string_of_int !lc ^ ":
					 iconst_1\n");
					 lc := !lc + 1
				| BinPlus -> print_string "iadd\n"
				| BinMinus -> print_string "isub\n"
				| BinBitOr -> print_string  "ior\n"
				| BinBitXor -> print_string "ixor\n"
				| BinMult -> print_string "imul\n"
				| BinDiv -> print_string "idiv\n"
				| BinMod -> print_string "irem\n"
				| BinShiftLeft -> print_string "ishl\n"
				| BinShiftRight -> print_string "ishr\n"
				| BinBitAnd -> print_string "iand\n"
				| BinBitAndNot -> print_string " &^ " (* iand and flip all bits *)
			in 
			print_binop_int binop
		| GoFloat -> (* TO DO need to fix label jumping*)
			let print_binop_fl op = match op with
				| BinEq -> 
					print_string   (* use double jasmin instr. *)
					("fcmpg
					 ifeq Label_" ^ string_of_int !lc ^
					 "iconst_0\nLabel_" ^ string_of_int !lc ^
					 ":\niconst_1\n");
					 lc := !lc + 1
				| BinNotEq -> print_string 
					("fcmpg
					 ifne Label_" ^ string_of_int !lc ^
					 "iconst_0\nLabel_" ^ string_of_int !lc ^ 
					 ":\niconst_1\n");
					 lc := !lc + 1
				| BinLess -> print_string 
					("fcmpg
					 iconst_m1
					 if_icmpeq Label_" ^ string_of_int !lc ^
					 "iconst_0
					  Label_" ^ string_of_int !lc ^
					  ":\niconst_1\n");
					 lc := !lc + 1
				| BinLessEq ->  () (* negation of > negation: if 0: 1 if != 0, 0*)
				| BinGreater -> print_string
					("fcmpg
					 iconst_1
					 if_icmpeq Label_" ^ string_of_int !lc ^
					 "iconst_0
					 Label_" ^ string_of_int !lc ^
					 ":\niconst_1\n");
					 lc := !lc + 1
				| BinGreaterEq -> () (* negation of < *)
				| BinPlus -> print_string " fadd "
				| BinMinus -> print_string " fsub "
				| BinMult -> print_string " fmul "
				| BinDiv -> print_string " fdiv "
				| BinMod -> print_string " frem "
			in 
			print_binop_fl binop
		| GoBool ->
			let print_binop_bool op = match op with
				| BinOr -> 
					begin
						print_string ("ifne Label_" ^ string_of_int !lc ^ 
						"\nifne Label_" ^ string_of_int !lc ^ "\niconst_1\n");
						lc := !lc + 1;
						print_string ("goto Label_" ^ string_of_int !lc ^ "\n");
						lc := !lc - 1;
						print_string ("Label_ " ^ string_of_int !lc ^
						":\niconst_0\n");
						lc := !lc + 1;
						print_string ("Label_ " ^ string_of_int !lc ^ ":\n");	
					end
				| BinAnd -> 
					begin
						print_string ("ifeq Label_" ^ string_of_int !lc ^
						"\nifeq Label_" ^ string_of_int !lc ^ "\niconst_1\n");
						lc := !lc + 1;
						print_string ("goto Label_" ^ string_of_int !lc ^ "\n");
						lc := !lc - 1;
						print_string ("Label_ " ^ string_of_int !lc ^
						":\niconst_0\n");
						lc := !lc + 1;
						print_string ("Label_ " ^ string_of_int !lc ^ ":\n");	
					end	
			in 
			print_binop_bool binop
	in
	(* Assume the result of the exp following the unary op is already on stack*)
	let print_unary_op (unop, typ) = match typ with
		| GoInt ->
			let print_uop_int op = match op with
				| UPlus -> ()
				| UMinus -> print_string "ineg "
				| UCaret -> print_string " ^ " (* xor with -1 depends ?*)
			in print_uop_int unop 
		| GoFloat -> (* use double jasmin instr. *)
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
				| UNot -> 
					begin
						print_string ("ifeq Label_" ^ string_of_int !lc ^ "iconst_0\n");
						lc := !lc + 1;
						print_string ("goto Label_" ^ string_of_int !lc ^ "\n");
						lc := !lc - 1;
						print_string ("iconst_1\nLabel_ " ^ string_of_int !lc ^ "\n");
						lc := !lc + 1;
						print_string ("Label_ " ^ string_of_int !lc ^ ":\n");	
					end
			in print_unop_bool unop
	in
	(* load from different places depending if id is from func arg or a local var*)

	(* WARNING : this function only print out the name of the id, only to be used in function calls,
	   for loading a identifier from a local var, we need another function *)
	let print_identifier id typ = match id with
		| ID (s, _) -> print_string s
		| BlankID -> ()
	in
	let print_int_literal lit = match lit with
		| Ast.DecInt (s) -> print_string ("iconst_" ^ s ^ "\n") (* only works for 0-5! *)
		| Ast.HexInt (s) -> ()
		| Ast.OctalInt (s) -> ()
	in
	let print_literal lit = match lit with
		| IntLit (i) -> print_int_literal i
		| FloatLit (f) -> print_string ("ldc " ^ f ^ "\n")
		| RuneLit (r) -> ()
		| StringLit (s) -> print_string ("ldc \"" ^ s ^ "\"\n")
		| RawStringLit (s) -> ()
	in
	let print_basic_type t = match t with
		| IntType -> print_string "I"
		| FloatType -> print_string "F"
		| BoolType -> ()
		| RuneType -> ()
		| StringType -> print_string "Ljava/lang/String;"
	in
	let print_go_type t_op = match t_op with
		| None -> print_string "V"
		| Some (t) -> (match t with
			| GoInt -> print_string "I"
			| GoFloat -> print_string "F"
			| GoBool -> ()
			| GoRune -> ()
			| GoString -> print_string "Ljava/lang/String;")
	in
	(* type_spec only used in func declarations *)
	let print_type_spec ts = match ts with
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
	(* print function args in function calls*)
	let rec print_func_call_args args_list = match args_list with
		| [] -> ()
		| h::[] -> (match h with 
			| Expression(exp, typ) -> print_go_type !typ)
		| h::t ->  
			(match h with 
			| Expression(exp, typ) -> print_go_type !typ);
			print_func_call_args t
	in
	(* Leave the result of the expression on top of the stack, increasing the stack height by 1. *)
	let rec print_expr (Expression(exp, typ)) = match !typ with
		| None -> ()
		| Some (t) -> match exp with
			(* IdExp only works to print the function name in function calls. Will need to be changed.*)
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
			| FunctionCallExp (Expression(exp, typ), args_list) ->
				(match !typ with
				| None -> raise (InternalError "expression should have a type")
				| Some (t) -> (match t with
					| GoFunction(arg_type, ret_typ)->
						begin
							print_string ("invokestatic " ^ class_name ^ "/");
							print_expr (Expression(exp, typ));
							print_string "(";
							(*only need to print the type of the expressions*)
							print_func_call_args args_list; 
							print_string ")";
							print_go_type ret_typ;
							print_string "\n";
						end))
			| AppendExp(id, e) -> () (* TODO *)
			| TypeCastExp(ts, e) -> () (* TODO *)
			| IndexExp(e1, e2) -> () (* TODO *)
			| SelectExp(e, id) -> () (* TODO *) 
	in
	let print_multi_var_decl mvd = match mvd with
		| MultipleVarDecl (svd_list) -> ()  (* TODO *)
	in
	let print_var_decl mvd_list = ()  (* TODO *)
	in
	let print_short_var_decl svd_list  = ()  (* TODO *)
	in
	(*since print instruction depends on the type of each expression,
	 every expr need a print instruction*)
	let print_println_stmt_helper (Expression(e, tp)) = match !tp with
		| None -> raise (InternalError "expression should always have a type")
		| Some (t) -> (match t with
			| GoInt -> 
				begin
					print_expr (Expression(e, tp));
					print_string "invokevirtual java/io/PrintStream/println(I)V\n";
				end
				
			| GoFloat -> 
				begin
					print_expr (Expression(e, tp));
					print_string "invokevirtual java/io/PrintStream/println(F)V\n";
				end
			| GoString -> 
				begin
					print_expr (Expression(e, tp));
					print_string "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n"					
				end
			(*need to print according to the return type of the function *)
			| GoFunction (arg_type, ret_typ) -> 
				begin
					print_expr (Expression(e, tp));
					print_string "invokevirtual java/io/PrintStream/println(I)V\n";
				end				
		)
	in
	(* very repetitive, these two helper methods need to be merged. *)
	let print_print_stmt_helper (Expression(e, tp)) = match !tp with
		| None -> raise (InternalError "expression should always have a type")
		| Some (t) -> (match t with
			| GoInt -> 
				begin
					print_expr (Expression(e, tp));
					print_string "invokevirtual java/io/PrintStream/print(I)V\n";
				end
				
			| GoFloat -> 
				begin
					print_expr (Expression(e, tp));
					print_string "invokevirtual java/io/PrintStream/print(F)V\n";
				end
			| GoString -> 
				begin
					print_expr (Expression(e, tp));
					print_string "invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n"					
				end
			(*need to print according to the return type of the function *)
			| GoFunction (arg_type, ret_typ) -> 
				begin
					print_expr (Expression(e, tp));
					print_string "invokevirtual java/io/PrintStream/print(I)V\n";
				end				
		)
	in
	let rec print_stmt stmt = 
		match stmt with
		| EmptyStatement -> ()
		| ExpressionStatement (e) -> print_expr e
		| AssignmentStatement (l)-> ()
		| TypeDeclBlockStatement (decl_list)-> ()
		| VarDeclBlockStatement (decl_list)-> ()
		| ShortVarDeclStatement(decl_list)-> ()
		| PrintStatement (e_list)-> 
			begin
				print_string "getstatic java.lang.System.out Ljava/io/PrintStream;\n";
				List.iter print_print_stmt_helper e_list;
			end
		| PrintlnStatement (e_list)-> 
			begin
				print_string "getstatic java.lang.System.out Ljava/io/PrintStream;\n";
				List.iter print_println_stmt_helper e_list;
			end
		| PrintlnStatement (e_list)-> ()
		| IfStatement (stmtop, e, stmts, stmtsop) ->
			(match stmtop with
			| None -> ()
			| Some(stmt) -> print_stmt_wrap stmt);
			print_expr e;
			let label = !lc in
			lc := !lc + 1;
			print_string ("ifeq Label_" ^ string_of_int label ^ "\n");
			List.iter (fun x -> print_stmt_wrap x) stmts;
			print_string ("Label_" ^ string_of_int label ^ ": \n");
			(match stmtsop with
			| None -> ()
			| Some(stmts) -> List.iter (fun x -> print_stmt_wrap x) stmts)
		| ReturnStatement (e_op)-> 
			(match e_op with
			| None -> print_string "return\n"
			| Some (Expression(exp, typ)) ->
				(match !typ with
				| None -> raise (InternalError "should have a function return type")
				| Some (t) -> match t with
					| GoInt -> 
						begin
							print_expr (Expression(exp, typ));
							print_string "ireturn\n"
						end
					| GoFloat -> 
						begin
							print_expr (Expression(exp, typ));
							print_string "freturn\n"
						end))
		| SwitchStatement (s_op,e,case_list)-> ()
		| ForStatement (s1_op,e_op,s2_op,stmt_list)-> ()
		| BreakStatement -> ()
		| ContinueStatement  -> ()
		| BlockStatement (sl) -> ()
	and print_stmt_wrap stmt  = match stmt with
		| LinedStatement (ln,s) -> print_stmt s
	in
	(* class initialization, not sure what class name would be*)
	let print_init_class classname = print_string 
		(".source " ^ class_name ^ 
		 ".go\n.class public " ^ class_name ^
		 "\n.super java/lang/Object\n\n") 
	in
	let print_top_decl td = match td with 
		| FunctionDecl (id, fs, stmt_list) -> 
			begin
				(match id with 
				| ID (s, _) ->
					begin
						print_string (".method public static " ^ s);
						print_func_sign fs;
						print_string ".limit stack 100\n.limit locals 100\n\n";
						List.iter print_stmt_wrap stmt_list;
					end);
				(*add a return instruction if the function has no return type.*)
				(match fs with
					| FunctionSig (f_list, t_op) -> match t_op with
						| None -> print_string "return\n";
						| Some (t) -> () 
				);
				print_string (".end method\n\n");
			end

		| TypeDeclBlock (tl) -> ()
		| VarDeclBlock (vl) -> ()
	in
	let print_lined_top_decl_list ldl = 
		List.iter 
			(fun x -> 
				let LinedTD(td, _) = x in 
				let () = print_top_decl td in
				print_string "\n") ldl
	in
	print_init_class class_name;
	(* print_init_method; *)
	print_lined_top_decl_list ldl;
	close_out outfile


(* add args and 'return' in main*)
