(* let level = ref 0

let print_ast (pack, dl) pretty = 
	let outfile = open_out pretty in 
	let print_string = fun s -> output_string outfile s in
	let insert_tab () = 
		print_string (String.make !level '\t')
	in
	let print_binop binop = match binop with
		| Ast.BinOr -> print_string "||"
		| Ast.BinAnd -> print_string "&&"
		| Ast.BinEq -> print_string "=="
		| Ast.BinNotEq -> print_string "!="
		| Ast.BinLess -> print_string "<"
		| Ast.BinLessEq -> print_string "<="
		| Ast.BinGreater -> print_string ">"
		| Ast.BinGreaterEq -> print_string ">="
		| Ast.BinPlus -> print_string "+"
		| Ast.BinMinus -> print_string "-"
		| Ast.BinBitOr -> print_string  "|"
		| Ast.BinBitXor -> print_string "^"
		| Ast.BinMult -> print_string "*"
		| Ast.BinDiv -> print_string "/"
		| Ast.BinMod -> print_string "%"
		| Ast.BinShiftLeft -> print_string "<<"
		| Ast.BinShiftRight -> print_string ">>"
		| Ast.BinBitAnd -> print_string "&"
		| Ast.BinBitAndNot -> print_string "&^"
	in
	let print_unary_op unop = match unop with
		| Ast.UPlus -> print_string "+"
		| Ast.UMinus -> print_string "-"
		| Ast.UNot -> print_string "!"
		| Ast.UCaret -> print_string "^"
	in
	let print_identifier id = match id with
		| Ast.IdName (s) -> print_string s
		| Ast.BlankID -> print_string "_"
	in
	let print_int_literal lit = match lit with
		| Ast.DecInt (s) -> print_string s
		| Ast.HexInt (s) -> print_string s
		| Ast.OctalInt (s) -> print_string s
	in
	let print_literal lit = match lit with
		| Ast.IntLit (i) -> print_int_literal i
		| Ast.FloatLit (f) -> print_string f
		| Ast.RuneLit (r) -> print_string r
		| Ast.StringLit (s) -> print_string s
	in
	let print_basic_type t = match t with
		| Ast.IntType -> print_string "int"
		| Ast.FloatType -> print_string "float64"
		| Ast.BoolType -> print_string "bool"
		| Ast.RuneType -> print_string "rune"
		| Ast.StringType -> print_string "string"
	in 
	let rec print_struct_field_decl sfd = match sfd with
		| Ast.StructFieldDecl (sf_list) -> 
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
		| Ast.StructField (id, ty) ->
			begin
				print_identifier id;
				print_type_spec ty;
			end
	and print_type_spec ts = match ts with
		| Ast.BasicType (bt) -> print_basic_type bt
		| Ast.SliceType (t) -> 
			begin
				print_string "[";
				print_string "]";
				print_type_spec t;
			end
		| Ast.ArrayType (int_lit, t) ->
			begin
				print_string "[";
				print_int_literal int_lit;
				print_string "]";
				print_type_spec t;
			end
		| Ast.StructType (st) -> 
			begin
				print_string "struct { ";
				List.iter print_struct_field_decl st; 
				print_string "}";
			end

		| Ast.CustomType (id) -> print_identifier id
	in
	let rec print_expr exp = match exp with
		| Ast.IdExp (i) -> print_identifier i
		| Ast.LiteralExp (l) -> print_literal l
		| Ast.UnaryExp (op, e) -> 
			begin
				print_string "(";
				print_unary_op op;
				print_expr e;
				print_string ")";
			end
		| Ast.BinaryExp (op,e1,e2) ->
			begin
				print_string "(";
				print_expr e1;
				print_binop op;
				print_expr e2;
				print_string ")";
			end
		| Ast.FunctionCallExp (id,e_list) ->
			begin
				print_identifier id;
				print_string "(";
				List.iter print_expr e_list;
				print_string ")";
			end
		| Ast.AppendExp (id,e) ->
			begin
				print_string "append";
				print_string "(";
				print_identifier id;
				print_string ",";
				print_expr e;
				print_string ")";
			end
		| Ast.TypeCastExp (t,e) ->
			begin
				print_type_spec t;
				print_string "(";
				print_expr e;
				print_string ")";
			end
		| Ast.IndexExp (e1,e2) ->
			begin
				print_expr e1;
				print_string "[";
				print_expr e2;
				print_string "]";
			end
		| Ast.SelectExp (e,id) ->
			begin
				print_expr e;
				print_string ".";
				print_identifier id;
			end
	in
	let rec print_lvalue lvalue = match lvalue with 
		| Ast.LId (i) -> print_identifier i
		| Ast.LIndex (lv,e) -> 
			begin
				print_lvalue lvalue;
				print_string "[";
				print_expr e;
				print_string "]"
			end
		| Ast.LSelector (lv,i)->
			begin
				print_lvalue lv;
				print_string ".";
				print_identifier i;
			end
	in
	let print_func_arg fa = match fa with
		| Ast.FunctionArg (id,t) ->
			begin
				print_identifier id;
				print_type_spec t;
			end
	in
	let print_func_sign fs = match fs with
		| Ast.FunctionSig (f_list, t_op) ->
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
		| Ast.SingleTypeDecl (id,ts) ->
			begin
				print_identifier id;
				print_type_spec ts;
			end
	in
	let print_single_var_decl vd = match vd with 
		| Ast.SingleVarDecl (id, t_op, e_op) ->
			begin
				print_identifier id;
				(match t_op with 
				| None -> ()
				| Some t_op -> print_type_spec t_op);
				(match e_op with 
				| None -> ()
				| Some e_op -> print_expr e_op);
				print_string ",";
			end
	in
	let print_var_decl vd = match vd with 
		| Ast.MultipleVarDecl (svd_list) -> List.iter print_single_var_decl svd_list
	in
	let rec print_stmt stmt = 
		insert_tab ();
		match stmt with
		| Ast.EmptyStatement -> ()
		| Ast.ExpressionStatement (e) -> print_expr e;
		| Ast.AssignmentStatement (l)->
			begin
			 	List.iter (fun (lv,e) -> 
			 		begin
				 		print_lvalue lv;
			 			print_string "=";
			 			print_expr e;			
			 		end
			 	) l
			 end 
		| Ast.TypeDeclBlockStatement (decl_list)-> List.iter print_type_decl decl_list
		| Ast.VarDeclBlockStatement (decl_list)-> List.iter print_var_decl decl_list
		| Ast.PrintStatement (e_list)-> 
			begin
				print_string "print ";
				print_string "(";
				List.iter print_expr e_list;
				print_string ")";
			end
		| Ast.PrintlnStatement (e_list)->
			begin
				print_string "print ";
				print_string "(";
				List.iter print_expr e_list;
				print_string ")";
				print_string "\n";
			end
		| Ast.IfStatement (s, e, s1_list, s2_list) ->
			begin
				print_string "if ";
				(match s with 
				| None -> ()
				| Some s -> print_stmt s);
				print_string ";";
				print_expr e;
				print_string " { \n";
				level := !level + 1;
				List.iter print_stmt s1_list;
				level := !level - 1;
				insert_tab ();
				print_string "} else { \n";
				match s2_list with 
				| None -> print_string "}"
				| Some s2_list -> List.iter print_stmt s2_list
			end
		| Ast.ReturnStatement (e_op)-> 
		    (match e_op with
			| None -> print_string "return;"
			| Some e_op -> 
				begin
					print_string "return";
					print_expr e_op;
					print_string ";"
				end)
		| Ast.SwitchStatement (s_op,e,case_list)->
			begin
				print_string "switch";
				(match s_op with 
				|None -> ()
				|Some s_op -> print_stmt s_op);
				print_string ";";
				print_expr e;
				print_string "{ \n";
				let print_case_list sl = 
					(match sl with
					| Ast.SwitchCase (e_list,case_list) -> 
						begin
							print_string "case: ";
							List.iter print_stmt case_list;
						end
					| Ast.DefaultCase (case_list) -> 
						begin
							print_string "default: \n";
							List.iter print_stmt case_list;
						end
					)
				in
				List.iter print_case_list case_list;
			end

		| Ast.ForStatement (s1_op,e,s2_op,stmt_list)->
			begin
				print_string "for";
				match s1_op with
				| None ->()
				| Some s1_op -> print_stmt s1_op;
				print_string ";";
				print_string "true";
				match s2_op with
				| None ->()
				| Some s2_op -> print_stmt s2_op;
				print_string ";";
				print_string "{ ";
				List.iter print_stmt stmt_list;
				print_string "}";
			end
		| Ast.BreakStatement -> print_string "break;"
		| Ast.ContinueStatement  -> print_string "continue;"
	in
	let print_func_decl fd = match fd with 
		| Ast.Function (id,fs, stmt_list) ->
			begin
				print_string "func";
				print_identifier id;
				print_func_sign fs;
				List.iter print_stmt stmt_list;
			end
	in

	let print_top_decl td = match td with
		| Ast.FunctionDecl (f) -> print_func_decl f
		| Ast.TypeDeclBlock (tl) -> List.iter print_type_decl tl
		| Ast.VarDeclBlock (vl) -> List.iter print_var_decl vl
	in

	let print_top_decl_list dl =
		List.iter print_top_decl dl 
	in
	let print_package s = match s with
		| Ast.Package (str) -> 
			begin
				print_string "package";
				print_string str;
			end
	in 
	print_package pack;
	print_top_decl_list dl;
	close_out outfile *)