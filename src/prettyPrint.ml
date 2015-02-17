let level = ref 0

let print_ast (dl, sl) pretty = 
	let outfile = open_out pretty in 
	let print_string = fun s -> output_string outfile s in
	let print_int = fun i ->  output_string outfile (string_of_int i) in
	let print_float = fun f -> output_string outfile (string_of_float f) in
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
		| Ast.UPlus ->
		| Ast.UMinus ->
		| Ast.UNot -> 
		| Ast.UCaret ->
	in
	let print_lvalue lvalue = match lv with 
		| Ast.LId (i) ->
		| Ast.LIndex (lv,e) ->
		| Ast.LSelector (lv,i)->
	in
	let print_int_literal lit = match lit with
		| Ast.DecInt (s) ->
		| Ast.HexInt (s) ->
		| Ast.OctalInt (s) ->
	in
	let print_literal lit = match lit with
		| Ast.IntLit (i) ->
		| Ast.FloatLit (f) ->
		| Ast.RuneLit (r) ->
		| Ast.StringLit (s) ->
	in
	let print_expr exp = match exp with
		| Ast.IdExp (i) ->
		| Ast.LiteralExp (l) ->
		| Ast.UnaryExp (op, e) ->
		| Ast.BinaryExp (op,e1,e2) ->
		| Ast.FunctionCallExp (id,e_list) ->
		| Ast.AppendExp (id,e) ->
		| Ast.TypeCastExp (t,e) ->
		| Ast.IndexExp (e1,e2) ->
		| Ast.SelectExp (e,id) ->
	in
	let print_func_arg fa = match fa with
		| Ast.FunctionArg (id,t) ->
	in
	let print_func_sign fs = match fs with
		| Ast.FunctionSig (fa_list, t_op) ->
	in
	let print_func_decl fd = match fd with 
		| Ast.Function (id,fs, stmt_list) ->
	in
	let print_identifier id = match id with
		| Ast.IdName (s) ->
		| Ast.BlankID ->
	in
	let print_basic_type t = match t with
		| Ast.IntType ->
		| Ast.FloatType ->
		| Ast.BoolType ->
		| Ast.RuneType ->
		| Ast.StringType ->
	in
	let print_struct_field sf = match sf with
		| Ast.StructField (id, ty) ->
	in 
	let print_struct_field_decl sfd = match sfd with
		| Ast.StructFieldDecl (sf_list) -> List.iter print_struct_field sf_list
	in
	let print_type_spec ts = match ts with
		| Ast.BasicType (bt) ->
		| Ast.SliceType (t) ->
		| Ast.ArrayType (at) ->
		| Ast.StructType (st) ->
		| Ast.CustomType (id) ->
	in
	let print_type_decl td = match td with
		| Ast.SingleTypeDecl (id,ts) ->
	in
	let print_single_var_decl vd = match vd with 
		| Ast.SingleVarDecl (id, t_op, e_op) ->
	in
	let print_var_decl vd = match vd with 
		| Ast.MultiplevarDecl (svd_list) -> List.iter print_single_var_decl svd_list
	in
	let print_top_decl td = match td with
		| Ast.FunctionDecl (f) ->
		| Ast.TypeDeclBlock (t) ->
		| Ast.VarDeclBlock (v) ->
	in
	let insert_tab () = 
		print_string (String.make !level '\t')
	in
	let rec print_stmt stmt = 
		insert_tab ();
		match stmt with
		| Ast.EmptyStatement () -> 
		| Ast.ExpressionStatement (e) ->
			print_expr e;
		| Ast.AssignmentStatement (list(lv,e))-> 
		| Ast.TypeDeclBlockStatement (decl_list)->
		| Ast.VarDeclBlockStatement (decl_list)->
		| Ast.PrintStatement (e_list)->
		| Ast.PrintlnStatement (e_list)->
		| Ast.IfStmt (e, s_list) ->
			begin
				print_string "if ";
				print_expr e;
				print_string " then \n";
				level := !level + 1;
				List.iter print_stmt s_list;
				level := !level - 1;
				insert_tab ();
				print_string "endif \n";
			end
		| Ast.ReturnStatement (e_op)->
		| Ast.SwitchStatement (s_op,e,case_list)->
		| Ast.SwitchCase (e_list,st_list) ->
		| Ast.DefaultCase (st_list) ->
		| Ast.ForStatement (s1_op,e,s2_op,stmt_list)->
		| Ast.BreakStatement () ->
		| Ast.ContinueStatement () ->
	in



		let print_top_decl_list dl =
			List.iter print_decl dl 
		in
		print_package s = match s with
			| Ast.Package (str) -> print_string str;
		print_top_decl sl;
		close_out outfile