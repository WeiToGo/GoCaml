open JasminAst
open Ast
open Symtable

(* exceptions *)
exception ImproperType
exception NotImplemented
exception InternalError of string

(* global counters *)
let next_bool_exp_count = Utils.new_counter 0

(* helper functions *)

let string_of_id id = match id with
| ID(name, _) -> name 
| BlankID -> raise (InternalError("You attempted to convert a blank id to string. This can only bring you misery.")) 

let exp_type (Expression(_, type_ref)) = match !type_ref with
| None -> raise (InternalError("Empty expression type in AST. Did you typecheck the AST?"))
| Some t -> t

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
| GoFloat -> JFloat
| GoBool -> JInt
| GoRune -> JChar  (* TODO: Make sure this is okay *)
| GoString -> JRef(jc_string)
| GoArray(_, t) -> JArray(get_jvm_type t)
| GoStruct(_) -> raise NotImplemented
| GoFunction(_) -> raise (InternalError("You tried to get the jvm_type of a go function. Sadly, the legion of anti-functional programmers explicitly forbids this act.")) 
| GoCustom(_, t) -> get_jvm_type t
| GoSlice(_) -> raise NotImplemented
| NewType(t) -> raise (InternalError("Custom types suffer from existential crisis in jvm bytecode."))

let get_mapping_from_stmt next_index stmt = LocalVarMap.empty

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
| FloatLit(s) -> [JInst(Ldc(s))]
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
| _ -> print_string "expression not implemented"; raise NotImplemented

and process_binary_expression op e1 e2 = 
  let e1_insts = process_expression e1 in 
  let e2_insts = process_expression e2 in
  match exp_type e1 with
  | GoInt ->
      let label_serial = next_bool_exp_count () in 
      let true_label = "True_" ^ (string_of_int label_serial) in
      let false_label = "False_" ^ (string_of_int label_serial) in 
      let end_label = "EndBoolExp_" ^ (string_of_int label_serial) in
      e1_insts @ e2_insts @
      (match op with
      | BinEq -> 
        [ JInst(ICmpeq(true_label));
          JLabel(false_label);
          JInst(Iconst_0);
          JInst(Goto(end_label));
          JLabel(true_label);
          JInst(Iconst_1);
          JLabel(end_label);
        ]
      | BinNotEq -> 
        [ JInst(ICmpne(true_label));
          JLabel(false_label);
          JInst(Iconst_0);
          JInst(Goto(end_label));
          JLabel(true_label);
          JInst(Iconst_1);
          JLabel(end_label);
        ]
      | BinLess ->
       [ JInst(ICmplt(true_label));
          JLabel(false_label);
          JInst(Iconst_0);
          JInst(Goto(end_label));
          JLabel(true_label);
          JInst(Iconst_1);
          JLabel(end_label);
        ]
      | BinLessEq ->
       [ JInst(ICmple(true_label));
          JLabel(false_label);
          JInst(Iconst_0);
          JInst(Goto(end_label));
          JLabel(true_label);
          JInst(Iconst_1);
          JLabel(end_label);
        ]
      | BinGreater ->
       [ JInst(ICmpgt(true_label));
          JLabel(false_label);
          JInst(Iconst_0);
          JInst(Goto(end_label));
          JLabel(true_label);
          JInst(Iconst_1);
          JLabel(end_label);
        ]
      | BinGreaterEq ->
       [ JInst(ICmpge(true_label));
          JLabel(false_label);
          JInst(Iconst_0);
          JInst(Goto(end_label));
          JLabel(true_label);
          JInst(Iconst_1);
          JLabel(end_label);
        ]
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
      (* | BinBitAndNot -> raise NotImplemented *)
      )
  | _ -> print_string "Unimplemented binary operation"; raise NotImplemented


let process_var_decl mvd_list = (* raise NotImplemented *)
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


let rec process_statement (LinedStatement(_, s)) = match s with
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
| ExpressionStatement(e) -> process_expression(e) @ [JInst(Pop)]

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

  let merge_maps k xo yo = match xo,yo with
  | Some x, Some y -> raise (InternalError("Same variable present in two maps. This should be impossible."))
  | Some x, None -> xo
  | None, Some y -> yo
  | None, None -> None in
  let FunctionSig(fun_args, _) = funsig in
  let arg_ids = List.map (fun (FunctionArg(id, _)) -> id) fun_args in
  let map_with_args = 
        List.fold_left
          (fun old_map id -> 
            let _, gt, varnum = id_info id in 
            LocalVarMap.add varnum (next_index (), get_jvm_type gt) old_map)
          LocalVarMap.empty
          arg_ids
  in
  let maps_from_stmts = List.map (get_mapping_from_stmt next_index) stmt_list in
  let local_mapping = 
    List.fold_left
      (fun old_map new_map -> LocalVarMap.merge merge_maps old_map new_map)
      map_with_args
      maps_from_stmts in
  let stmt_code = List.flatten (List.map process_statement stmt_list) in
  let code = if signature.return_type = JVoid then stmt_code @ [JInst(Return)] else stmt_code in (* PerfPenalty *)
  { signature; code; local_mapping; }

let process_top_level_decl (LinedTD(top_decl, _)) = match top_decl with
| FunctionDecl(id, funsig, stmt_list) -> ([], [process_func_decl id funsig stmt_list])
| TypeDeclBlock(_) -> ([], [])  (* Ignore type declarations. *)
| VarDeclBlock(mvd_list) -> (process_var_decl mvd_list, [])

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
