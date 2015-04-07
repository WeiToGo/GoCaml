open JasminAst
open Ast
open Symtable

(* exceptions *)
exception ImproperType
exception NotImplemented
exception InternalError of string

(* global counters *)
let next_bool_exp_count = Utils.new_counter 0
let next_loop_count = Utils.new_counter 0

(* helper functions *)

let string_of_id id = match id with
| ID(name, _) -> name 
| BlankID -> raise (InternalError("You attempted to convert a blank id to string. This can only bring you misery.")) 

let exp_type (Expression(_, type_ref)) = match !type_ref with
| None -> raise (InternalError("Empty expression type in AST. Did you typecheck the AST?"))
| Some t -> t

let merge_maps k xo yo  = match xo,yo with
| Some x, Some y -> raise (InternalError("Same variable present in two maps. This should be impossible."))
| Some x, None -> xo
| None, Some y -> yo
| None, None -> None
(* Returns name, gotype, and serial number of id *)
let id_info id = 
  let sym_table_entry = match id with
  | ID(_, entry_ref) -> !entry_ref
  | BlankID -> raise (InternalError("Go home blank id.")) in
  match sym_table_entry with
  | None -> raise (InternalError("Empty id type in AST. Did you typecheck the AST?"))
  | Some(entry) -> 
    let Symtable.Entry(name, gotype, _, _, var_num) = entry in
    name, gotype, var_num
(*~~ read deal ~~*)

let rec get_jvm_type gotype = match gotype with
| GoInt -> JInt
| GoFloat -> JDouble
| GoBool -> JInt
| GoRune -> JChar  (* TODO: Make sure this is okay *)
| GoString -> JRef(jc_string)
| GoArray(_, t) -> JArray(get_jvm_type t)
| GoStruct(_) -> raise NotImplemented
| GoFunction(_) -> raise (InternalError("Trick question - functions don't have types in jvm.")) 
| GoCustom(_, t) -> get_jvm_type t
| GoSlice(_) -> raise NotImplemented
| NewType(t) -> raise (InternalError("Custom types suffer from existential crisis in jvm bytecode."))

let get_local_var_decl_mappings next_index mvd_list = 
  let mapping_from_svd svd = 
    let SingleVarDecl(id, _, _) = svd in 
    let _, gotype, var_num = id_info id in 
    (var_num, (next_index (), get_jvm_type gotype))
  in 
  let mapping_from_mvd mvd = 
    let MultipleVarDecl(svd_list) = mvd in 
    List.map mapping_from_svd svd_list
  in
  let map_list = List.flatten (List.map mapping_from_mvd mvd_list) in 
  List.fold_left
    (fun old_map (var_num, local_entry) -> LocalVarMap.add var_num local_entry old_map)
    LocalVarMap.empty
    map_list


let rec get_mapping_from_stmt prev_map next_index (LinedStatement(_, stmt)) = match stmt with
| VarDeclBlockStatement(mvd_list) -> get_local_var_decl_mappings next_index mvd_list
| ShortVarDeclStatement(shortvd_list) -> 
    let mapping_from_single_shvd (ShortVarDecl(id, _)) = 
      let _, gotype, var_num = id_info id in
      if LocalVarMap.mem var_num prev_map then LocalVarMap.empty 
      else LocalVarMap.add var_num (next_index (), get_jvm_type gotype) LocalVarMap.empty
    in 
    List.fold_left 
      (fun old_map new_map -> 
        LocalVarMap.merge merge_maps old_map new_map)
      LocalVarMap.empty
      (List.map mapping_from_single_shvd shortvd_list)
| IfStatement(tiny_stmt, _, then_stmts, else_stmts_op) -> raise NotImplemented
| SwitchStatement(tiny_stmt, _, switch_case_list) -> raise NotImplemented
| ForStatement(init_stmt_op, _, post_stmt_op, stmt_list) -> 
    let init_mapping = match init_stmt_op with
    | None -> LocalVarMap.empty
    | Some s -> get_mapping_from_stmt prev_map next_index s 
    in 
    let body_mapping = 
      List.fold_left 
        (LocalVarMap.merge merge_maps)
        LocalVarMap.empty
        (List.map (get_mapping_from_stmt prev_map next_index) stmt_list)
    in 
    LocalVarMap.merge merge_maps init_mapping body_mapping 
| BlockStatement(stmt_list) -> raise NotImplemented
| EmptyStatement
| ExpressionStatement _
| AssignmentStatement _ 
| TypeDeclBlockStatement _ 
| PrintStatement _ 
| PrintlnStatement _ 
| ReturnStatement _ 
| BreakStatement
| ContinueStatement -> LocalVarMap.empty

let get_local_mapping_from_go_arg (FunctionArg (id, _)) = 
  let name, gotype, var_num = id_info id in 
  (var_num, get_jvm_type gotype)


let process_literal = function
| StringLit(s) -> [JInst(Ldc(quote_string s))] 
| IntLit(DecInt(s)) -> [JInst(Ldc(s))]
| IntLit(OctalInt(s)) -> 
    let int_repr = int_of_string ("0o" ^ s) in 
    [JInst(Ldc(string_of_int int_repr))]
| IntLit(HexInt(s)) -> 
    let int_repr = int_of_string s in
    [JInst(Ldc(string_of_int int_repr))]
| FloatLit(s) -> [JInst(Ldc2w(s))]
| _ -> raise NotImplemented



let rec process_expression (Expression(e, t)) = match e with 
| LiteralExp(lit) -> process_literal lit
| IdExp(id) -> 
    let _, _, var_num = id_info id in
    [PS(LoadVar(var_num))]
| FunctionCallExp(fun_expression, arg_expressions) -> 
    let Expression(fun_exp, _) = fun_expression in
    let fun_name, fun_type, _ = (match fun_exp with
    | IdExp(id) -> id_info id
    | _ -> raise (InternalError("Only identifiers can be called."))  )
    in 
    let arg_gotypes, ret_gotypeop = match fun_type with
    | GoFunction(at, rto) -> at, rto 
    | _ -> raise (InternalError("Only function types can be called. Do we have a typechecker bug?"))
    in 
    let jfunction_sig = 
      { method_name = main_class_name ^ "/" ^ fun_name;
        arg_types = List.map get_jvm_type arg_gotypes;
        return_type = match ret_gotypeop with
        | None -> JVoid 
        | Some t -> get_jvm_type t ;
      } in 
    let arg_load_instructions = List.map process_expression arg_expressions in
    let stack_null_instrtuctions = match ret_gotypeop with
    | None -> [JInst(AConstNull)]
    | Some _ -> [] in  
      (List.flatten arg_load_instructions) @ 
      [JInst(InvokeStatic(jfunction_sig))] @
      stack_null_instrtuctions
| BinaryExp(op, e1, e2) -> process_binary_expression op e1 e2
| UnaryExp(op, e) -> process_unary_expression op e
| _ -> print_string "expression not implemented"; raise NotImplemented

and process_binary_expression op e1 e2 = 
  let e1_insts = process_expression e1 in 
  let e2_insts = process_expression e2 in
  let label_serial = next_bool_exp_count () in 
  let true_label = "True_" ^ (string_of_int label_serial) in
  let true2_label = "True2_" ^ (string_of_int label_serial) in
  let false_label = "False_" ^ (string_of_int label_serial) in 
  let false2_label = "False2_" ^ (string_of_int label_serial) in 
  let end_label = "EndBoolExp_" ^ (string_of_int label_serial) in
  let true_false_boilerplate = 
    [ JLabel(false_label);
      JInst(Iconst_0);
      JInst(Goto(end_label));
      JLabel(true_label);
      JInst(Iconst_1);
      JLabel(end_label);
    ] in 
  e1_insts @ e2_insts @
  match exp_type e1 with
  | GoInt ->
      (match op with
      | BinEq -> 
        [ JInst(ICmpeq(true_label)) ] @ true_false_boilerplate
      | BinNotEq -> 
        [ JInst(ICmpne(true_label)) ] @ true_false_boilerplate
      | BinLess ->
       [ JInst(ICmplt(true_label)) ] @ true_false_boilerplate
      | BinLessEq ->
       [ JInst(ICmple(true_label)) ] @ true_false_boilerplate
      | BinGreater ->
       [ JInst(ICmpgt(true_label)) ] @ true_false_boilerplate
      | BinGreaterEq ->
       [ JInst(ICmpge(true_label)) ] @ true_false_boilerplate
      | BinPlus -> [JInst(Iadd);]
      | BinMinus -> [JInst(Isub);]
      | BinMult -> [JInst(Imul);]
      | BinDiv -> [JInst(Idiv);]
      | BinMod -> [JInst(Irem);]
      | BinBitOr -> [JInst(Ior);]
      | BinBitXor -> [JInst(Ixor);]
      | BinShiftLeft -> [JInst(Ishl)]
      | BinShiftRight -> [JInst(Ishr);]
      | BinBitAnd -> [JInst(Iand);]
      | BinOr|BinAnd| BinBitAndNot -> raise NotImplemented
      )
  | GoFloat -> 
      (match op with
      | BinEq -> 
        [ JInst(DCmpg);
          JInst(Ifeq(true_label)); ]
        @ true_false_boilerplate
      | BinNotEq ->
        [ JInst(DCmpg);
          JInst(Ifne(true_label)); ]
        @ true_false_boilerplate
      | BinLess ->
        [ JInst(DCmpg);
          JInst(Iconst_m1);
          JInst(ICmpeq(true_label)); ]
        @ true_false_boilerplate
      | BinLessEq -> 
        [ JInst(DCmpg);
          JInst(Iconst_1);
          JInst(ICmpne(true_label)); ]
        @ true_false_boilerplate
      | BinGreater ->
        [ JInst(DCmpg);
          JInst(Iconst_1);
          JInst(ICmpeq(true_label)); ]
        @ true_false_boilerplate
      | BinGreaterEq ->
        [ JInst(DCmpg);
          JInst(Iconst_m1);
          JInst(ICmpne(true_label)); ]
        @ true_false_boilerplate
      | BinPlus -> [JInst(Dadd);]
      | BinMinus -> [JInst(Dsub);]
      | BinMult -> [JInst(Dmul);]
      | BinDiv -> [JInst(Ddiv);]
      | BinMod -> [JInst(Drem);] (* not supported in Go*)
      | BinBitOr
      | BinBitXor
      | BinShiftLeft
      | BinShiftRight
      | BinBitAnd
      | BinOr|BinAnd| BinBitAndNot -> raise NotImplemented
      )
  | GoBool ->
      (match op with
      | BinOr -> (* only [0 0] is false*)
        [ JInst(Ifne(true_label));
          JInst(Ifne(true2_label));
          JLabel(false_label);
          JInst(Iconst_0);
          JInst(Goto(end_label));
          JLabel(true_label);
          JInst(Pop);
          JInst(Iconst_1);
          JInst(Goto(end_label));
          JLabel(true2_label);
          JInst(Iconst_1);
          JLabel(end_label);
        ]
      | BinAnd -> (* only [1 1] is true*)
        [ JInst(Ifeq(false_label));
          JInst(Ifeq(false2_label));
          JLabel(true_label);
          JInst(Iconst_1);
          JInst(Goto(end_label));
          JLabel(false_label);
          JInst(Pop);
          JInst(Iconst_0);
          JInst(Goto(end_label));
          JLabel(false2_label);
          JInst(Iconst_0);
          JLabel(end_label);
        ]
      | _ -> raise NotImplemented
    )
  | _ -> print_string "Unimplemented binary operation"; raise NotImplemented


let process_global_var_decl mvd_list =
  let mapping_from_svd svd = 
    let SingleVarDecl(id, tp_op, exp_op) = svd in
    let name, gotype, var_num = id_info id in
    let init_code = match exp_op with
    | None -> []
    | Some(e) -> (process_expression e) @ [PS(StoreVar(var_num))] in 
    { name = name ^ "_" ^ (string_of_int var_num);
      var_number = var_num;
      jtype = get_jvm_type gotype;
      init_code = init_code; } 
  in
  let mapping_from_mvd mvd = 
    let MultipleVarDecl(svd_list) = mvd in
    List.map mapping_from_svd svd_list
  in
  List.flatten (List.map mapping_from_mvd mvd_list)

(* Returns the initialization code for all the variable declarations *)
let get_local_var_decl_instructions mvd_list = 
  let insts_from_svd svd = 
    let SingleVarDecl(id, tp_op, exp_op) = svd in 
    let _, _, var_num = id_info id in 
    match exp_op with
    | None -> []
    | Some e -> (process_expression e) @ [PS(StoreVar(var_num))]
  in 
  let insts_from_mvd mvd = 
    let MultipleVarDecl(svd_list) = mvd in 
    List.flatten (List.map insts_from_svd svd_list)
  in
  List.flatten (List.map insts_from_mvd mvd_list)

let print_single_expression print_method_name e = 
  JInst(GetStatic(jc_sysout, JRef(jc_printstream))) :: 
  (match exp_type e with
  | GoBool -> process_expression e @ 
      [ JInst(InvokeStatic(jcr_booltostring));
        JInst(InvokeVirtual({
          method_name = print_method_name;
          arg_types = [JRef(jc_string)];
          return_type = JVoid; } ) ) ]

  | _ -> process_expression e @ 
      [ JInst(InvokeVirtual(
            { method_name = print_method_name;
              arg_types = [get_jvm_type (exp_type e)]; 
              return_type = JVoid; } )); ] )


let rec process_statement ?break_label ?continue_label (LinedStatement(_, s)) = match s with
| PrintlnStatement(exp_list) -> 
    (* get instructions for each expression *)
    let print_instructions = 
      List.map 
      (print_single_expression jc_println)
      exp_list in 
    List.flatten print_instructions
| PrintStatement(exp_list) -> 
    let print_instructions = 
      List.map 
      (print_single_expression jc_print)
      exp_list in 
    List.flatten print_instructions
| VarDeclBlockStatement(mvd_list) -> get_local_var_decl_instructions mvd_list
| TypeDeclBlockStatement _ -> [] (* Ignore type declarations *)
| ExpressionStatement(e) -> process_expression(e) @ [JInst(Pop)]
| EmptyStatement -> []
| BlockStatement(stmt_list) -> List.flatten (List.map process_statement stmt_list)
| ReturnStatement(e_op) -> raise NotImplemented
| ForStatement(init_stmt_op, loop_cond_op, post_stmt_op, stmt_list) -> 
    let count = string_of_int (next_loop_count ()) in 
    let loop_check_label = "LoopCheck_" ^ count in 
    let loop_begin_label = "LoopBegin_" ^ count in
    let loop_post_label = "LoopPost_" ^ count in 
    let loop_end_label = "LoopEnd_" ^ count in   
    let init_instructions = match init_stmt_op with
    | None -> []
    | Some s -> process_statement s in 
    let post_instructions = match post_stmt_op with
    | None -> []
    | Some s -> process_statement s in 
    let cond_statements = match loop_cond_op with
    | None -> [ JInst(Iconst_1) ]  (* This while true block *)
    | Some(e) -> process_expression e in 
    let body_instructions = 
      List.flatten 
        (List.map 
          (process_statement ~break_label:loop_end_label ~continue_label:loop_post_label)
          stmt_list)
    in 
    init_instructions @ 
    [ JLabel(loop_check_label) ] @
    cond_statements @
    [ JInst(Ifeq(loop_end_label));
      JLabel(loop_begin_label) ] @
    body_instructions @
    [ JLabel(loop_post_label) ] @
    post_instructions @ 
    [ JInst(Goto(loop_check_label));
      JLabel(loop_end_label) ]
| ShortVarDeclStatement(shortvd_list) -> 
    (* first evaluate all the arguments, then store them *)
    let exps = List.map (fun (ShortVarDecl(id, e)) -> e) shortvd_list in 
    let vars = List.map 
      (fun (ShortVarDecl(id, e)) -> 
        let _, _, var_num = id_info id in var_num)
      shortvd_list
    in
    let exp_instructions = List.flatten (List.map process_expression exps) in 
    let store_instructions = 
      List.map
        (fun var_num -> PS(StoreVar(var_num)))
        (List.rev vars)
    in
    exp_instructions @ store_instructions
| AssignmentStatement(ass_list) -> 
    let lvals = List.map (fun (e1, e2) -> e1) ass_list in 
    let exps = List.map (fun (e1, e2) -> e2) ass_list in 
    let exp_instructions = List.flatten (List.map process_expression exps) in
    let single_store_instruction (Expression(e, _)) = match e with
    | IdExp(id) -> 
        let _, _, var_num = id_info id in 
        [PS(StoreVar(var_num))]
    | IndexExp(id) -> raise NotImplemented
    | SelectExp(id) -> raise NotImplemented
    | FunctionCallExp(id) -> raise NotImplemented
    | AppendExp _ | TypeCastExp _ | LiteralExp _ 
    | UnaryExp _ | BinaryExp _  -> raise (InternalError("This is not a valid lvalue"))
    in
    let store_instructions = 
      List.flatten 
        (List.map single_store_instruction (List.rev lvals))
    in
    exp_instructions @ store_instructions
| BreakStatement -> ( match break_label with
  | None -> raise (InternalError("I know not whence to break"))
  | Some l -> [ JInst(Goto(l)) ] )
| ContinueStatement -> ( match continue_label with
  | None -> raise (InternalError("I know not whence to continue"))
  | Some l -> [ JInst(Goto(l)) ] )
| _ -> print_string "statement not implemented"; raise NotImplemented

let process_func_decl id funsig stmt_list = 
  let next_index = Utils.new_counter 0 in 
  let method_name, gotype, _ = id_info id in 
  let go_arg_types, go_ret_type_op = match gotype with
  | GoFunction(atlist, reto) -> atlist, reto
  | _ -> raise (InternalError("This should definitely have been a function."))
  in 
  let signature = if method_name = "main" then 
    {
      method_name = "main";
      arg_types = [JArray(JRef(jc_string))];
      return_type = JVoid;
    } else
    { 
      method_name = method_name;
      arg_types = List.map get_jvm_type go_arg_types;
      return_type = match go_ret_type_op with
      | None -> JVoid
      | Some(t) -> get_jvm_type (t); 
    } in 

  let FunctionSig(fun_args, _) = funsig in
  let arg_ids = List.map (fun (FunctionArg(id, _)) -> id) fun_args in
  let map_with_args = 
        List.fold_left
          (fun old_map id -> 
            let _, gt, var_num = id_info id in 
            LocalVarMap.add var_num (next_index (), get_jvm_type gt) old_map)
          LocalVarMap.empty
          arg_ids
  in
  let local_mapping = 
    List.fold_left 
      (fun prev_map stmt -> 
        LocalVarMap.merge 
          merge_maps 
          prev_map
          (get_mapping_from_stmt prev_map next_index stmt) ) 
      map_with_args
      stmt_list in
  let stmt_code = List.flatten (List.map process_statement stmt_list) in
  let code = if signature.return_type = JVoid then stmt_code @ [JInst(Return)] else stmt_code in (* PerfPenalty *)
  { signature; code; local_mapping; }

let process_top_level_decl (LinedTD(top_decl, _)) = match top_decl with
| FunctionDecl(id, funsig, stmt_list) -> ([], [process_func_decl id funsig stmt_list])
| TypeDeclBlock(_) -> ([], [])  (* Ignore type declarations. *)
| VarDeclBlock(mvd_list) -> (process_global_var_decl mvd_list, [])

let create_byte_code_ast (Program(_, lined_top_decls)) go_filename = 
  let field_method_nest = List.map process_top_level_decl lined_top_decls in 
  let rev_tlvars, rev_methods = 
    List.fold_left
      (fun (rtlvars, rmethods) (tlvlist, method_list) -> 
        (List.rev_append tlvlist rtlvars, List.rev_append method_list rmethods))
      ([], [])
      field_method_nest in 
  { 
    source = go_filename;
    top_level_vars = List.rev rev_tlvars; 
    methods = List.rev rev_methods;
  }
