open Ast

exception InternalError of string

let calculate_stack_height (Program(_,ldl)) = 	
	let rec range a b =
	    if a > b then []
	    else a :: range (a+1) b
	in
	let rec max_of_list l = (match l with
      | [] ->  raise (InternalError ("max_of_list called on an empty list"))
      | [h] -> h
      | h :: t -> max h (max_of_list t)
      )
	in
	(*find the maximum intermediate height in an expression. *)
	let rec calculate_expression go_expression = 
		(match go_expression with
		| Expression(exp, _) -> (match exp with
			| IdExp(id) -> 1
			| LiteralExp(lit) ->  1
			| UnaryExp(_, exp) -> calculate_expression exp
			| BinaryExp(_, e1, e2) -> 
				let e1_height = calculate_expression e1 in
				let e2_height = calculate_expression e2 in
				max (e1_height +1) (e2_height + 1)
			(*most Invoke_ instructions = object ref + list of argument, 
			for foo(a1, a2, a3) -> max of (count_a1 + 1, count_a2 +2, count_a3+3)*)
			| FunctionCallExp(e, exp_list) -> 
				let len = List.length exp_list in
				if len > 0 then max_of_list (calculate_max_list exp_list calculate_expression)	
				else 1		
			| AppendExp(id, exp) -> calculate_expression exp (* to check*)
			| TypeCastExp(ts, exp) -> calculate_expression exp
			| IndexExp(e1, e2) ->
				let e1_height = calculate_expression e1 in
				let e2_height = calculate_expression e2 in
				max (e1_height +1) (e2_height + 1)
			| SelectExp(e, id) ->
				calculate_expression e + 1
			)
		)

	(* helper function, given an expression list l = (e1, e2, e3),
	and a function, returns max ((f e1)+1, (f e2)+2, (f e3)+3 )*)
	and calculate_max_list l f = 
		let len = List.length l in 
		let list_index = range 1 len in
		let max_list = List.map f l in
		List.map2 (fun x y -> x+y) list_index max_list
	in
	let rec calculate_plain_statement go_stmt = (match go_stmt with
	    | EmptyStatement | BreakStatement | ContinueStatement -> 0
	    | ExpressionStatement(exp) -> calculate_expression exp
	    | AssignmentStatement(asm_list) -> 
	    	let lhs_exps = List.map (fun (x, y) -> x) asm_list in
	    	let rhs_exps = List.map (fun (x, y) -> y) asm_list in
			let left_list = List.map calculate_expression lhs_exps in
			let max_left = max_of_list left_list in  
	    	let max_right = max_of_list (calculate_max_list rhs_exps calculate_expression) in
	    	max_left + max_right
	    | TypeDeclBlockStatement(typ_decl_list) -> 0
	    | ShortVarDeclStatement(svd_list) ->
    	 	let rhs_exps = 
    	 		List.map (fun (ShortVarDecl(id, exp)) -> exp) svd_list
    	 	in
    	 	max_of_list (calculate_max_list rhs_exps calculate_expression)
	    | VarDeclBlockStatement(mvd_list) -> 
	    	let calc_single_var_decl svd = (match svd with
	    	| SingleVarDecl(_, _, exp_op) -> (match exp_op with
	    		| None -> 0
	    		| Some(e) -> calculate_expression e 
	    		)
	    	)
	    	in
	    	let calc_multiple_var_decl (MultipleVarDecl(svd_list)) = 
	    		max_of_list (List.map calc_single_var_decl svd_list)
	    	in
	    	max_of_list (List.map calc_multiple_var_decl mvd_list)
	    | PrintStatement(exp_list) ->
	    	let exp_count = max_of_list (calculate_max_list exp_list calculate_expression)
	    	in exp_count + 2 (*2 for getstatic *)
	    | PrintlnStatement(exp_list) -> 
	     	let exp_count = max_of_list (calculate_max_list exp_list calculate_expression)
	    	in exp_count + 2 (*2 for getstatic *)
	    (*find max stack height between s_op, exp, sl, and sl_op*)
	    | IfStatement(s_op, exp, sl, sl_op) ->
	    	let max_stmt_init = (match s_op with
	    	| None -> 0
	    	| Some(s) -> calculate_statement s
	    	) in
	    	let max_exp = calculate_expression exp in
	    	let max_if_stmt = 
	    		max_of_list (List.map calculate_statement sl)
	    	in
	    	let max_else_list = (match sl_op with 
	    	| None -> 0
	    	| Some (sl) -> max_of_list (List.map calculate_statement sl)
	    	)
	    	in
	    	let total_max_list = 
	    		[max_stmt_init;
	    		 max_exp;
	    		 max_if_stmt; 
	    		 max_else_list] in
	    	max_of_list total_max_list
	    | ReturnStatement(exp_op) -> (match exp_op with
	    	| None -> 0
	    	| Some(e) -> calculate_expression e
	    )
	    (*max of s_op, exp, for each case_expr: case_expression + 1,
	    , each case_statement and default statement. *)
	    | SwitchStatement(s_op, exp, case_list) ->
	    	let max_stmt_init = (match s_op with
	    	| None -> 0
	    	| Some(s) -> calculate_statement s
	    	) in
	    	let max_expression = calculate_expression exp in
	    	let max_single_case case = (match case with
	    	(*for switch case, find max between e1+1, e2+1, ..en+1 and stmt_list*)
	    	| SwitchCase(exp_list, st_list) ->
	    		let max_exp = max_of_list (calculate_max_list exp_list calculate_expression) in
	    		let max_stmt = max_of_list (List.map calculate_statement st_list) in
	    		if max_exp > max_stmt then max_exp else max_stmt
	    	| DefaultCase(st_list) -> 
	    		max_of_list (List.map calculate_statement st_list)
	    	)
	    	in
	    	let max_case_list = max_of_list (List.map max_single_case case_list)
	    	in
	     	let total_max_list = [max_stmt_init; max_expression; max_case_list] in
	    	max_of_list total_max_list
	    | ForStatement (s1_op, exp_op, s2_op, st_list) ->
	    	let max_stmt_init = (match s1_op with
	    	| None -> 0
	    	| Some(s) -> calculate_statement s
	    	) in
	    	let max_exp = (match exp_op with
	    	| None -> 0
	    	| Some(e) -> calculate_expression e
	    	) in
	    	let max_post_loop = (match s2_op with
	    	| None -> 0
	    	| Some(s) -> calculate_statement s
	    	) in
	    	let max_stmt = max_of_list (List.map calculate_statement st_list) in
	     	let total_max_list = 
	    		[max_stmt_init; max_exp; max_post_loop; max_stmt] in
	    	max_of_list total_max_list    	
	    | BlockStatement (stmt_list) -> max_of_list (List.map calculate_statement stmt_list)
		)
	and calculate_statement (LinedStatement(_, stmt)) = calculate_plain_statement stmt
	in
	(*It takes statement list in a function and returns the max stack height. *)
	let calculate_function stmt_list = 
		max_of_list (List.map calculate_statement stmt_list)
	in
	let calculate_top_decl td = (match td with
	| FunctionDecl(id, fs, stmt_list) -> calculate_function stmt_list
	| _ -> 0)
	in
	let calc_lined_top_decl_list ldl = 
		let calc_line_top_decl (LinedTD (td, _)) =
			calculate_top_decl td 
		in
		max_of_list (List.map calc_line_top_decl ldl)
	in

	calc_lined_top_decl_list ldl 
