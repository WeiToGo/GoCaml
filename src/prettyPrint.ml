open Ast

exception NotImplemented 

let print_ast prog pretty level = 
	let Program(pack,ldl) = prog in
	let outfile = open_out pretty in 
	let print_string = fun s -> output_string outfile s in
	let print_char = fun c -> output_char outfile c in
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
		| ID (s, _) -> print_string s 
		| BlankID -> print_string "_ "
	in
	let print_int_literal lit = match lit with
		| Ast.DecInt (s) -> print_string s
		| Ast.HexInt (s) -> print_string s
		| Ast.OctalInt (s) -> print_string s
	in
	let print_escape_chars_help c = match c with
			| '\007' -> print_string "\\a"
			| '\010' -> print_string "\\b"
			| '\014' -> print_string "\\f"
			| '\012' -> print_string "\\n"
			| '\015' -> print_string "\\r"
			| '\011' -> print_string "\\t"
			| '\013' -> print_string "\\v"
			| '\\' -> print_string "\\\\"
			| '\'' -> print_string "\\\'"
			| '\"' -> print_string "\\\""
			| _ -> print_char c
	in
	let str_to_char_list str = 
		let rec explode i char_list = 
			if i < 0 then char_list else explode (i-1) (str.[i] :: char_list) in
		explode (String.length str - 1) []
	in
	let print_literal lit = match lit with
		| IntLit (i) -> print_int_literal i
		| FloatLit (f) -> print_string f
		| RuneLit (r) -> 
			begin
				print_string "\'";
				print_escape_chars_help (List.hd (str_to_char_list r));
				print_string "\'";
			end
		| StringLit (s) -> 
			begin
				print_string "\"";
				List.iter print_escape_chars_help (str_to_char_list s);
				print_string "\"";
			end
		| RawStringLit (s) -> 
			begin
				print_string "`";
				print_string s;
				print_string "`";
			end
	in
	let print_basic_type t = match t with
		| IntType -> print_string " int"
		| FloatType -> print_string " float64"
		| BoolType -> print_string " bool"
		| RuneType -> print_string " rune"
		| StringType -> print_string " string"
	in
	let rec print_multi_struct_field level msf= match msf with
		| MultipleStructFieldDecl (ssf_list) ->
			let svd_proj1 = function SingleStructFieldDecl(id, ty) -> id in
			let svd_proj2 = function SingleStructFieldDecl(id, ty) -> ty in
			let ids = List.map svd_proj1 ssf_list in
			let tys = List.map svd_proj2 ssf_list in
			let rec list_printer lyst printer_fun = match lyst with
			| a :: (b :: _ as t)  ->(
									 printer_fun a;
									 print_string ", ";
								 	 list_printer t printer_fun
								 	)
			| a :: [] -> printer_fun a;
			| [] -> ()
		  in
		  insert_tab(level);
		  list_printer ids print_identifier;
		  print_type_spec level (List.hd tys);
		  print_string ";\n";
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
				let rec print_func_call_helper l = (match l with
					| [] -> ()
					| h::[] -> print_expr level h
					| h::t ->
						begin
							print_expr level h;
							print_string ", ";
							print_func_call_helper t;
						end)
				in
				print_func_call_helper e_list;
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
						print_func_sign_helper t;
					end 
				in
				print_func_sign_helper f_list;
				print_string ")";
				match t_op with 
					| None -> print_string ""
					| Some t_op -> print_type_spec level t_op
			end
	in
	let print_single_type_decl level td = match td with
		| SingleTypeDecl (id,ts) ->
			begin
				insert_tab(level);
				print_identifier id;
				print_type_spec level ts;
			end
	in
	let print_type_decl level tdl = match tdl with
		| [] -> ()
		| h ::[] -> 
			insert_tab(level);
			print_string "type ";
			print_single_type_decl level h;
		| h::t ->	
			insert_tab(level);
			print_string "type (\n";
			List.iter 
			(fun x -> 
				print_single_type_decl (level+1) x;
				print_string ";\n";
			) 
			tdl;
			insert_tab(level);
			print_string ")";
	in
	let print_multi_var_decl level mvd = match mvd with
	| MultipleVarDecl (svd_list) -> 
		insert_tab(level);
		let proj1 = function SingleVarDecl(id, ty_op, exp_op) -> id in
		let proj2 = function SingleVarDecl(id, ty_op, exp_op) -> ty_op in
		let proj3 = function SingleVarDecl(id, ty_op, exp_op) -> exp_op in
		let ids = List.map proj1 svd_list in
		let types = List.map proj2 svd_list in
		let exps = List.map proj3 svd_list in
		let rec list_printer lyst printer_fun = match lyst with
			| a :: (b :: _ as t)  ->(
									 printer_fun a;
									 print_string ", ";
								 	 list_printer t printer_fun
								 	)
			| a :: [] -> printer_fun a;
			| [] -> ()
		in
		let rec all_exists sth_list = match sth_list with
		| Some(_) :: t -> all_exists t
		| None :: t -> false
		| [] -> true
		in
		let () = list_printer ids print_identifier in
		match all_exists types, all_exists exps with
		| false, false -> failwith "Internal error: Invalid variable declaration"
		| true, false -> (
			let var_type = (match List.hd types with
				| Some(t) -> t
				| None -> failwith "This should never happen" )
			in 
			print_type_spec level var_type;
		)
		| false, true -> (
			print_string " = ";
			let extract_from_some exp = (match exp with
				| Some(e) -> e
				| None -> failwith "This should never happen")
			in
			list_printer (List.map extract_from_some exps) (print_expr level);
		)
		| true, true -> (
			let var_type = (match List.hd types with
								| Some(t) -> t
								| None -> failwith "This should never happen" )
			in 
			print_type_spec level var_type;
			print_string " = ";
			let extract_from_some exp = (match exp with
				| Some(e) -> e
				| None -> failwith "This should never happen")
			in
			list_printer (List.map extract_from_some exps) (print_expr level);
		)
	in
	let print_var_decl level mvd_list = 
		insert_tab(level);
		print_string "var (\n";
		List.iter 
			(fun x -> 
				print_multi_var_decl (level+1) x;
				print_string ";\n";
			) 
			mvd_list;
		insert_tab(level);
		print_string ")";
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
		| a :: [] -> printer_fun a;
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
			let proj1 = function (e1, e2) -> e1 in
			let proj2 = function (e1, e2) -> e2 in
			let e1s = List.map proj1 l in
			let e2s = List.map proj2 l in
			let rec list_printer lyst printer_fun = match lyst with
			| a :: (b :: _ as t)  ->(
									 printer_fun a;
									 print_string ", ";
								 	 list_printer t printer_fun
								 	)
			| a :: [] -> printer_fun a;
			| [] -> ()
			in
			insert_tab(level);
			list_printer e1s (print_expr level);
			print_string " = ";
			list_printer e2s (print_expr level); 
		| TypeDeclBlockStatement (decl_list)-> print_type_decl level decl_list
		| VarDeclBlockStatement (decl_list)->  print_var_decl (level) decl_list
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
				|Some s_op -> 
					begin
						print_stmt_wrap level s_op;
						print_string "; ";
					end);
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
		| ForStatement (s1_op,e_op,s2_op,stmt_list)->
			begin
				insert_tab(level);
				print_string "for ";
				(match s1_op with
				| None -> ()
				| Some s1_op -> 
					print_stmt_wrap level s1_op);
				print_string "; ";
				match e_op with
				| None -> ()
				| Some(e) -> print_expr level e;
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
	let print_package s = match s with
		| Package (str, _) -> 
			begin
				print_string "package ";
				print_string str;
				print_string ";\n\n"
			end
	in 
	print_package pack;
	print_lined_top_decl_list level ldl;
	close_out outfile


