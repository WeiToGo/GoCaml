open Ast

let print_ast prog pretty level = 
	let Program(pack,dl) = prog in
	let outfile = open_out pretty in 
	let print_string = fun s -> output_string outfile s in
	let insert_tab (level) = 
		print_string (String.make level '\t')
	in
	let print_binop binop = match binop with
		| BinOr -> print_string "||"
		| BinAnd -> print_string "&&"
		| BinEq -> print_string "=="
		| BinNotEq -> print_string "!="
		| BinLess -> print_string "<"
		| BinLessEq -> print_string "<="
		| BinGreater -> print_string ">"
		| BinGreaterEq -> print_string ">="
		| BinPlus -> print_string "+"
		| BinMinus -> print_string "-"
		| BinBitOr -> print_string  "|"
		| BinBitXor -> print_string "^"
		| BinMult -> print_string "*"
		| BinDiv -> print_string "/"
		| BinMod -> print_string "%"
		| BinShiftLeft -> print_string "<<"
		| BinShiftRight -> print_string ">>"
		| BinBitAnd -> print_string "&"
		| BinBitAndNot -> print_string "&^"
	in
	let print_unary_op unop = match unop with
		| UPlus -> print_string "+"
		| UMinus -> print_string "-"
		| UNot -> print_string "!"
		| UCaret -> print_string "^"
	in
	let print_identifier id = match id with
		| IdName (s) -> print_string (s ^ " ")
		| BlankID -> print_string "_ "
	in
	let print_int_literal lit = match lit with
		| Ast.DecInt (s) -> print_string s
		| Ast.HexInt (s) -> print_string s
		| Ast.OctalInt (s) -> print_string s
in
	let print_literal lit = match lit with
		| IntLit (i) -> print_int_literal i
		| FloatLit (f) -> print_string f
		| RuneLit (r) -> print_string r
		| StringLit (s) -> print_string s
	in
	let print_basic_type t = match t with
		| IntType -> print_string "int"
		| FloatType -> print_string "float64"
		| BoolType -> print_string "bool"
		| RuneType -> print_string "rune"
		| StringType -> print_string "string"
	in 
	let rec print_struct_field_decl sfd = match sfd with
		| StructFieldDecl (sf_list) -> 
				let rec print_struct_field_helper sl = match sl with
					| [] -> ()
					| h::[] -> print_struct_field h
					| h::t ->
					begin
						print_struct_field h;
						print_string ",";
						print_struct_field_helper t;
					end 
				in print_struct_field_helper sf_list
	and print_struct_field sf = match sf with
		| StructField (id, ty) ->
			begin
				print_identifier id;
				print_type_spec ty;
			end
	and print_type_spec ts = match ts with
		| BasicType (bt) -> print_basic_type bt
		| SliceType (t) -> 
			begin
				print_string "[";
				print_string "]";
				print_type_spec t;
			end
		| ArrayType (int_lit, t) ->
			begin
				print_string "[";
				print_int_literal int_lit;
				print_string "]";
				print_type_spec t;
			end
		| StructType (st) -> 
			begin
				print_string "struct { ";
				ignore(level = level + 1);
				insert_tab(level);
				List.iter print_struct_field_decl st; 
				ignore(level = level - 1);
				insert_tab(level);
				print_string "}";
			end
		| CustomType (id) -> print_identifier id
	in
	let rec print_expr exp = match exp with
		| IdExp (i) -> print_identifier i
		| LiteralExp (l) -> print_literal l
		| UnaryExp (op, e) -> 
			begin
				print_string "(";
				print_unary_op op;
				print_expr e;
				print_string ")";
			end
		| BinaryExp (op,e1,e2) ->
			begin
				print_string "(";
				print_expr e1;
				print_binop op;
				print_expr e2;
				print_string ")";
			end
		| FunctionCallExp (exp,e_list) ->
			begin
				print_expr exp;
				print_string "(";
				List.iter print_expr e_list;
				print_string ")";
			end
		| AppendExp (id,e) ->
			begin
				print_string "append ";
				print_string "(";
				print_identifier id;
				print_string ",";
				print_expr e;
				print_string ")";
			end
		| TypeCastExp (t,e) ->
			begin
				print_type_spec t;
				print_string "(";
				print_expr e;
				print_string ")";
			end
		| IndexExp (e1,e2) ->
			begin
				print_expr e1;
				print_string "[";
				print_expr e2;
				print_string "]";
			end
		| SelectExp (e,id) ->
			begin
				print_expr e;
				print_string ".";
				print_identifier id;
			end
	in
	let print_lvalue lvalue =
		print_expr (exp_of_lvalue lvalue)
	in 
	let print_func_arg fa = match fa with
		| FunctionArg (id,t) ->
			begin
				print_identifier id;
				print_type_spec t;
			end
	in
	let print_func_sign fs = match fs with
		| FunctionSig (f_list, t_op) ->
			begin
				print_string "(";
				let rec print_func_sign_helper l = match l with
					| [] -> ()
					| h::[] -> print_func_arg h
					| h::t ->
					begin
						print_func_arg h;
						print_string ",";
						print_func_sign_helper (t);
					end 
				in
				print_func_sign_helper f_list;
				print_string ")";
				match t_op with 
					| None -> print_string ""
					| Some t_op -> print_type_spec t_op
			end
	in
	let print_type_decl td = match td with
		| SingleTypeDecl (id,ts) ->
			begin
				print_identifier id;
				print_type_spec ts;
				print_string ";";
			end
	in
	let print_single_var_decl vd = match vd with 
		| SingleVarDecl (id, t_op, e_op) ->
			begin
				print_string "var ";
				print_identifier id;
				(match t_op with 
				| None -> ()
				| Some t_op -> print_type_spec t_op);
				(match e_op with 
				| None -> ()
				| Some e_op -> 
					begin
					print_string " = ";
					print_expr e_op;
					end)
			end
	in
	let print_var_decl vd = match vd with 
		| MultipleVarDecl (svd_list) -> 
				let rec print_var_decl_helper sl = (match sl with
					| [] -> ()
					| h::[] -> print_single_var_decl h
					| h::t ->
						begin
							print_single_var_decl h;
							print_string ",";
							print_var_decl_helper t;
						end);
					print_string ";";
				in print_var_decl_helper svd_list
	in
	let rec print_stmt level stmt = 
		match stmt with
		| EmptyStatement -> ()
		| ExpressionStatement (e) -> 
			begin 
				print_expr e;
				print_string ";";
			end
		| AssignmentStatement (l)->
			begin
			 	List.iter (fun (lv,e) -> 
			 		begin
				 		print_lvalue lv;
			 			print_string " = ";
			 			print_expr e;	
			 			print_string ";";		
			 		end
			 	) l
			 end 
		| TypeDeclBlockStatement (decl_list)-> List.iter print_type_decl decl_list
		| VarDeclBlockStatement (decl_list)-> List.iter print_var_decl decl_list
		| PrintStatement (e_list)-> 
			begin
				print_string "print ";
				print_string "(";
				List.iter print_expr e_list;
				print_string ");\n";
			end
		| PrintlnStatement (e_list)->
			begin
				print_string "println ";
				print_string "(";
				List.iter print_expr e_list;
				print_string ");\n";
			end
		| IfStatement (s, e, s1_list, s2_list) ->
			begin
				print_string "if ";
				(match s with 
				| None -> ()
				| Some s -> print_stmt_wrap level s);
				print_string ";";
				print_expr e;
				print_string " { \n";
				ignore(level = level + 1);
				insert_tab(level);
				List.iter (fun x -> print_stmt_wrap level x) s1_list;
				ignore(level = level - 1);
				insert_tab(level);
				print_string "} else { \n";
				match s2_list with 
				| None -> print_string "}; \n"
				| Some s2_list -> 
					begin
						ignore(level = level + 1);
						insert_tab(level); 
						List.iter (fun x -> print_stmt_wrap level x) s2_list;
					end
			end
		| ReturnStatement (e_op)-> 
		    (match e_op with
			| None -> print_string "return;\n"
			| Some e_op -> 
				begin
					print_string "return";
					print_expr e_op;
					print_string ";\n"
				end)
		| SwitchStatement (s_op,e,case_list)->
			begin
				print_string "switch";
				(match s_op with 
				|None -> ()
				|Some s_op -> print_stmt_wrap level s_op);
				print_expr e;
				print_string "{ \n";
				let print_case_list sl = 
					(match sl with
					| SwitchCase (e_list,case_list) -> 
						begin
							print_string "case: ";
							List.iter (fun x -> print_stmt_wrap level x) case_list;
						end
					| DefaultCase (case_list) -> 
						begin
							print_string "default: \n";
							List.iter (fun x -> print_stmt_wrap level x) case_list;
						end
					)
				in
				List.iter print_case_list case_list;
				print_string "} \n";
			end
		| ForStatement (s1_op,e,s2_op,stmt_list)->
			begin
				print_string "for";
				match s1_op with
				| None ->()
				| Some s1_op -> print_stmt_wrap level s1_op;
				match s2_op with
				| None ->()
				| Some s2_op -> print_stmt_wrap level s2_op;
				print_string "{\n ";
				List.iter (fun x -> print_stmt_wrap level x) stmt_list;
				print_string "}\n";
			end
		| BreakStatement -> print_string "break;\n"
		| ContinueStatement  -> print_string "continue;\n"
		| BlockStatement (sl) -> 
			begin
				print_string "{ \n";
				ignore(level = level + 1);
				insert_tab(level);
				List.iter (fun x -> print_stmt_wrap level x) sl;
				ignore(level = level - 1);
				insert_tab(level);
				print_string "}\n";
			end
	and print_stmt_wrap level stmt = match stmt with
		| LinedStatement (ln,s) -> print_stmt level s
	in
	let print_func_decl fd = match fd with 
		| Function (id,fs, stmt_list) ->
			begin
				print_string "func";
				print_identifier id;
				print_func_sign fs;
				print_string "{ \n";
				List.iter (fun x -> print_stmt_wrap level x) stmt_list;
				print_string "}; \n";
			end
	in
	let print_top_decl td = match td with
		| FunctionDecl (f) -> print_func_decl f
		| TypeDeclBlock (tl) -> List.iter print_type_decl tl
		| VarDeclBlock (vl) -> List.iter print_var_decl vl
	in

	let print_top_decl_list dl =
		List.iter print_top_decl dl 
	in
	let print_package s = match s with
		| Package (str) -> 
			begin
				print_string "package ";
				print_string str;
				print_string ";\n"
			end
	in 
	print_package pack;
	print_top_decl_list dl;
	close_out outfile


