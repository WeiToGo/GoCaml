open Ast

let print_ast prog pretty level = 
	let Program(pack,dl) = prog in
	let outfile = open_out pretty in 
	let print_string = fun s -> output_string outfile s in
	let insert_tab (level) = 
		print_string (String.make level '\t')
	in
	let print_binop binop = match binop with
		| BinOr -> print_string " || "
		| BinAnd -> print_string " && "
		| BinEq -> print_string " == "
		| BinNotEq -> print_string " != "
		| BinLess -> print_string " < "
		| BinLessEq -> print_string " <= "
		| BinGreater -> print_string " > "
		| BinGreaterEq -> print_string " >= "
		| BinPlus -> print_string " + "
		| BinMinus -> print_string " - "
		| BinBitOr -> print_string  " | "
		| BinBitXor -> print_string " ^ "
		| BinMult -> print_string " * "
		| BinDiv -> print_string " / "
		| BinMod -> print_string " % "
		| BinShiftLeft -> print_string " << "
		| BinShiftRight -> print_string " >> "
		| BinBitAnd -> print_string " & "
		| BinBitAndNot -> print_string " &^ "
	in
	let print_unary_op unop = match unop with
		| UPlus -> print_string " + "
		| UMinus -> print_string " - "
		| UNot -> print_string " ! "
		| UCaret -> print_string " ^ "
	in
	let print_identifier id = match id with
		| IdName (s) -> print_string s 
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
		| RuneLit (r) -> 
			begin
				print_string "'";
				print_string r;
				print_string "'";
			end
		| StringLit (s) -> 
			begin 
				print_string "\"";
				print_string s;
				print_string "\"";
			end
	in
	let print_basic_type t = match t with
		| IntType -> print_string " int"
		| FloatType -> print_string " float64"
		| BoolType -> print_string " bool"
		| RuneType -> print_string " rune"
		| StringType -> print_string " string"
	in 
	let rec print_multi_struct_field level sfd = match sfd with
		| MultipleStructFieldDecl (sf_list) -> 
				let rec print_struct_field_helper sl = match sl with
					| [] -> ()
					| h::[] -> 
						begin
							insert_tab(level);
							print_single_struct_field level h;
						end
					| h::t ->
					begin
						insert_tab(level);
						print_single_struct_field level h;
						print_string "; ";
						print_struct_field_helper t;
					end 
				in print_struct_field_helper sf_list;
				print_string ";\n";
	and print_single_struct_field level sf = match sf with
		| SingleStructFieldDecl (id, ty) ->
			begin
				print_identifier id;
				print_type_spec level ty;
			end
	and print_type_spec level ts = match ts with
		| BasicType (bt) -> print_basic_type bt
		| SliceType (t) -> 
			begin
				print_string "[";
				print_string "]";
				print_type_spec level t;
			end
		| ArrayType (int_lit, t) ->
			begin
				print_string "[";
				print_int_literal int_lit;
				print_string "]";
				print_type_spec level t;
			end
		| StructType (st) -> 
			begin
				print_string " struct {\n";
				List.iter (fun x -> print_multi_struct_field (level+1) x) st; 
				insert_tab(level);
				print_string "}";
			end
		| FunctionType (tl, ts_op) ->
			begin
				print_string " func ( ";
				List.iter (fun x -> print_type_spec level x) tl;
				print_string ") ";
				match ts_op with
				| None -> ()
				| Some ts_op ->  print_type_spec level ts_op
			end
		| CustomType (id) -> print_identifier id
	in
	let rec print_expr level exp = match exp with
		| IdExp (i) -> print_identifier i
		| LiteralExp (l) -> print_literal l
		| UnaryExp (op, e) -> 
			begin
				print_string "(";
				print_unary_op op;
				print_expr level e;
				print_string ")";
			end
		| BinaryExp (op,e1,e2) ->
			begin
				print_string "(";
				print_expr level e1;
				print_binop op;
				print_expr level e2;
				print_string ")";
			end
		| FunctionCallExp (exp,e_list) ->
			begin
				print_expr level exp;
				print_string "(";
				List.iter (fun x -> print_expr level x) e_list;
				print_string ")";
			end
		| AppendExp (id,e) ->
			begin
				print_string " append ";
				print_string "(";
				print_identifier id;
				print_string ",";
				print_expr level e;
				print_string ")";
			end
		| TypeCastExp (t,e) ->
			begin
				print_type_spec level t;
				print_string "(";
				print_expr level e;
				print_string ")";
			end
		| IndexExp (e1,e2) ->
			begin
				print_expr level e1;
				print_string "[";
				print_expr level e2;
				print_string "]";
			end
		| SelectExp (e,id) ->
			begin
				print_expr level e;
				print_string ".";
				print_identifier id;
			end
	in 
	let print_func_arg level fa = match fa with
		| FunctionArg (id,t) ->
			begin
				print_identifier id;
				print_type_spec level t;
			end
	in
	let print_func_sign level fs = match fs with
		| FunctionSig (f_list, t_op) ->
			begin
				print_string "(";
				let rec print_func_sign_helper l = match l with
					| [] -> ()
					| h::[] -> print_func_arg level h
					| h::t ->
					begin
						print_func_arg level h;
						print_string ", ";
						print_func_sign_helper (t);
					end 
				in
				print_func_sign_helper f_list;
				print_string ")";
				match t_op with 
					| None -> print_string ""
					| Some t_op -> print_type_spec level t_op
			end
	in
	let print_type_decl level td = match td with
		| SingleTypeDecl (id,ts) ->
			begin
				insert_tab(level);
				print_string "type ";
				print_identifier id;
				print_type_spec level ts;
				print_string ";\n";
			end
	in
	let print_single_var_decl level vd = match vd with 
		| SingleVarDecl (id, t_op, e_op) ->
			begin
				print_string "var ";
				print_identifier id;
				(match t_op with 
				| None -> ()
				| Some t_op -> print_type_spec level t_op);
				(match e_op with 
				| None -> ()
				| Some e_op -> 
					begin
					print_string " = ";
					print_expr level e_op;
					end)
			end
	in
	let print_var_decl level vd = match vd with 
		| MultipleVarDecl (mvd_list) -> 
				let rec print_var_decl_helper sl = (match sl with
					| [] -> ()
					| h::[] -> 
						begin 
							insert_tab(level);
							print_single_var_decl level h;
						end
					| h::t ->
						begin
							insert_tab(level);
							print_single_var_decl level h;
							print_string "; ";
							print_var_decl_helper t;
						end);
				in print_var_decl_helper mvd_list;
				print_string ";\n";
	in
	let print_short_var_decl level svd_list = 
		let svd_proj1 = function ShortVarDecl(id, exp) -> id in
		let svd_proj2 = function ShortVarDecl(id, exp) -> exp in
		let ids = List.map svd_proj1 svd_list in
		let exps = List.map svd_proj2 svd_list in
		let rec list_printer lyst printer_fun = match lyst with
		| a :: (b :: _ as t)  ->(
														 printer_fun a;
														 print_string ", ";
													 	 list_printer t printer_fun
													 	)
		| a :: [] -> print_string "a ";
		| [] -> ()
	  in
	  list_printer ids print_identifier;
	  print_string ":= ";
	  list_printer exps (print_expr level); 
	in
	let rec print_stmt level stmt = 
		match stmt with
		| EmptyStatement -> ()
		| ExpressionStatement (e) -> 
			begin 
				insert_tab(level);
				print_expr level e;
			end
		| AssignmentStatement (l)->
			begin
				insert_tab(level);
			 	List.iter (fun (e1,e2) -> 
			 		begin
				 		print_expr level e1;
			 			print_string " = ";
			 			print_expr level e2;	
			 		end
			 	) l
			 end 
		| TypeDeclBlockStatement (decl_list)-> 
			begin
				List.iter (fun x -> print_type_decl level x) decl_list;
			end
		| VarDeclBlockStatement (decl_list)-> 
			begin			
				List.iter (fun x -> print_var_decl level x) decl_list
			end
		| ShortVarDeclStatement(decl_list)->
			begin
				insert_tab(level);
				print_short_var_decl level decl_list
			end
		| PrintStatement (e_list)-> 
			begin
				insert_tab(level);
				print_string "print ";
				print_string "(";
				List.iter (fun x -> print_expr level x) e_list;
				print_string ")";
			end
		| PrintlnStatement (e_list)->
			begin
				insert_tab(level);
				print_string "println ";
				print_string "(";
				List.iter (fun x -> print_expr level x) e_list;
				print_string ")";
			end
		| IfStatement (s, e, s1_list, s2_list) ->
			begin
				insert_tab(level);
				print_string "if ";
				(match s with 
				| None -> ()
				| Some s -> print_stmt_wrap level s);
				print_string ";";
				print_expr level e;
				print_string " { \n";
				print_statement_list (level + 1) s1_list;
				insert_tab(level);
				print_string "} else { \n";
				match s2_list with 
				| None -> 
					begin
						insert_tab(level);
						print_string "}";
					end
				| Some s2_list -> 
					begin
						print_statement_list (level + 1) s2_list;
						insert_tab(level);
						print_string "}";
					end
			end
		| ReturnStatement (e_op)-> 	
			insert_tab(level);
		    (match e_op with
			| None -> print_string "return"
			| Some e_op -> 
				begin
					print_string "return ";
					print_expr level e_op;
				end)
		| SwitchStatement (s_op,e,case_list)->
			begin
				insert_tab(level);
				print_string "switch ";
				(match s_op with 
				|None -> ()
				|Some s_op -> print_stmt_wrap level s_op);
				print_expr level e;
				print_string " { \n";
				let print_case_list sl = 
					(match sl with
					| SwitchCase (e_list,stmt_list) -> 
						begin
							insert_tab(level);
							print_string "case ";
							List.iter (fun x -> print_expr level x) e_list;
							print_string ":";
							print_statement_list level stmt_list
						end
					| DefaultCase (stmt_list) -> 
						begin
							insert_tab(level);
							print_string "default: ";
							print_statement_list level stmt_list;
						end
					)
				in
				List.iter print_case_list case_list;
				insert_tab(level);
				print_string "}";
			end
		| ForStatement (s1_op,e,s2_op,stmt_list)->
			begin
				insert_tab(level);
				print_string "for ";
				(match s1_op with
				| None -> ()
				| Some s1_op -> 
					print_stmt_wrap level s1_op);
				print_string "; ";
				print_expr level e;
				print_string "; ";
				(match s2_op with
				| None ->()
				| Some s2_op -> print_stmt_wrap level s2_op);
				print_string " {\n ";
				print_statement_list (level + 1) stmt_list;
				insert_tab(level);
				print_string "}";
			end
		| BreakStatement -> 
			begin
				insert_tab(level);
				print_string "break"
			end
		| ContinueStatement  -> 
			begin
				insert_tab(level);
				print_string "continue"
			end
		| BlockStatement (sl) -> 
			begin
				insert_tab(level);
				print_string "{ \n";
				print_statement_list (level + 1) sl;
				insert_tab(level);
				print_string "}";
			end
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
				print_string "}; \n";
			end
		| TypeDeclBlock (tl) -> List.iter (fun x -> print_type_decl level x) tl
		| VarDeclBlock (vl) -> List.iter (fun x -> print_var_decl level x) vl
	in

	let print_top_decl_list level dl =
		List.iter (fun x -> print_top_decl level x) dl 
	in
	let print_package s = match s with
		| Package (str) -> 
			begin
				print_string "package ";
				print_string str;
				print_string ";\n\n"
			end
	in 
	print_package pack;
	print_top_decl_list level dl;
	close_out outfile


