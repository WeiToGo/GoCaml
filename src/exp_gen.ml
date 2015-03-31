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
					"fsub
					 dup
					 if_icmpne Label_" ^ lc
					 

					 "Label_" ^ lc ^ ":"
					 "iconst_0\n"
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
		end
	in 
	print_label_true;
	print_label_false;
	print_expr exp;
	close_out outfile
