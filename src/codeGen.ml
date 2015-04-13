open JasminAst
open Ast
open Symtable

(* exceptions *)
exception ImproperType
exception NotImplemented
exception InternalError of string
exception TypeCastError of string 

(* global counters *)
let next_bool_exp_count = Utils.new_counter 0
let next_loop_count = Utils.new_counter 0
let if_count = Utils.new_counter 0
let switch_count = Utils.new_counter 0
let array_init_count = Utils.new_counter 0
let negate_boolean_count = Utils.new_counter 0

let scmap = ref ( fun x -> raise (InternalError("StructMapReferenceVarNotInitializedError")) ) 

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


let rec base_type gotype = match gotype with
| GoCustom(_, t) -> base_type t
| NewType(t) -> base_type t
| _ -> gotype


let array_store_inst gotype = match base_type gotype with
| GoInt | GoBool | GoRune -> [JInst(IAstore)]
| GoFloat -> [JInst(DAstore)]
| GoString | GoArray _ | GoStruct _ | GoSlice _ -> [JInst(AAstore)]
| GoFunction(_) -> raise (InternalError("No function arrays allowed")) 
| GoCustom(_, t) -> raise (InternalError("No custom types allowed. Call base type."))
| NewType(t) -> raise (InternalError("No new types allowed"))

let array_load_inst gotype = match base_type gotype with
| GoInt | GoBool | GoRune -> [JInst(IAload)]
| GoFloat -> [JInst(DAload)]
| GoString | GoArray _ | GoStruct _ | GoSlice _ -> [JInst(AAload)]
| GoFunction(_) -> raise (InternalError("No function arrays allowed")) 
| GoCustom(_, t) -> raise (InternalError("No custom types allowed. Call base type."))
| NewType(t) -> raise (InternalError("No new types allowed"))


let negate_boolean () = 
  let count = string_of_int (negate_boolean_count ()) in 
  let it_was_false = "ItWasFalse_" ^ count in 
  let end_label = "EndNegateBoolean_" ^ count in 
  [ JInst(Ifeq(it_was_false)); 
    JInst(Iconst_0);
    JInst(Goto(end_label));
    JLabel(it_was_false);
    JInst(Iconst_1);
    JLabel(end_label); ] 


let rec get_jvm_type gotype = match gotype with
| GoInt -> JInt
| GoFloat -> JDouble
| GoBool -> JInt
| GoRune -> JInt  (*rune is an int-32*)
| GoString -> JRef(jc_string)
| GoArray(_, t) -> JRef(jc_list_class)
| GoStruct(sflist) -> JRef(!(scmap) sflist)
| GoFunction(_) -> raise (InternalError("Trick question - functions don't have types in jvm.")) 
| GoCustom(_, t) -> get_jvm_type t
| GoSlice(_) -> JRef(jc_list_class)
| NewType(t) -> raise (InternalError("Custom types suffer from existential crisis in jvm bytecode."))

let struct_cname_of_expression (Expression(e, topref)) = match !topref with 
| Some (t) -> (match (base_type t) with 
    | GoStruct(gs) -> !scmap gs
    | _ -> raise (InternalError("Select Expression can only be done on structs. This should be caught in typechecker.")) )
| None -> raise (InternalError("Missing type when trying to infer struct class name of expression. Did you typecheck the tree?"))


let wrap_type gotype = match base_type gotype with
| GoInt | GoRune | GoBool -> 
  [ JInst(New(jc_integer));
    JInst(Dup);
    JInst(Dup2_x1);
    JInst(Pop2);
    JInst(InvokeSpecial{ 
      method_name = flstring jc_integer "<init>";
      arg_types = [JInt]; return_type = JVoid;}); ]
| GoFloat -> 
  [ JInst(New(jc_double));
    JInst(Dup);
    JInst(Dup2_x2);
    JInst(Pop2);
    JInst(InvokeSpecial{ 
      method_name = flstring jc_double "<init>";
      arg_types = [JDouble]; return_type = JVoid;}); ]
| GoArray _ | GoStruct _ | GoString | GoSlice _ -> []
| GoCustom _ -> raise (InternalError("wrap: Cannot match custom type. Call base_type on type"))
| GoFunction _  | NewType _ -> invalid_arg "wrap type cannot take Newtype or function"


let unwrap_type gotype = match base_type gotype with 
| GoInt | GoRune | GoBool -> 
  [ JInst(CheckCast(jc_integer));
    JInst(InvokeVirtual{ 
      method_name = flstring jc_integer "intValue";
      arg_types = []; return_type = JInt;}); ]
| GoFloat -> 
  [ JInst(CheckCast(jc_double));
    JInst(InvokeVirtual{ 
      method_name = flstring jc_double "doubleValue";
      arg_types = []; return_type = JDouble;}); ]
| GoString -> [ JInst(CheckCast(jc_string))]
| GoArray _ | GoSlice _ -> [ JInst(CheckCast(jc_list_class)) ]
| GoStruct(s) -> [ JInst(CheckCast(!scmap s)) ]
| GoCustom _ -> raise (InternalError("unwrap: Cannot match custom type. Call base_type on type"))
| GoFunction _  | NewType _ -> invalid_arg "unwrap type cannot take Newtype or function"


let rec get_word_size gotype = match gotype with
| GoInt -> One
| GoFloat -> Two
| GoBool -> One
| GoRune -> One
| GoString -> One
| GoSlice _ -> One
| GoArray _ -> One
| GoStruct _ -> One
| GoFunction _ -> raise (InternalError("Functions are not put on stack"))
| GoCustom(s, t) -> get_word_size t
| NewType _ -> raise (InternalError("It is impossible to put newtype on stack"))

let rec type_init_exps gotype = match gotype with
| GoInt| GoRune | GoBool -> [JInst(Iconst_0)]
| GoFloat -> [JInst(Ldc2w("0.0"))]
| GoString -> [JInst(Ldc(quote_string ""))]
| GoStruct(gs) -> 
    let scname = (!scmap gs) in 
    [JInst(New(scname)); JInst(Dup);
     JInst(InvokeSpecial(
        { method_name = scname ^ "/<init>";
          arg_types = [];
          return_type = JVoid; }
     ))]
| GoCustom(_, t) -> type_init_exps t
| GoArray(size, t) ->
    let count = string_of_int (array_init_count ()) in 
    let comp_label = "ArrayInitComp_" ^ count in 
    let body_label = "ArrayInitElement_" ^ count in 
    let end_label = "ArrayInitEnd_" ^ count in 
    [ JInst(New(jc_list_class));
      JInst(Dup);
      JInst(InvokeSpecial({ 
        method_name = flstring jc_list_class "<init>";
        arg_types = []; return_type = JVoid; }));
      JInst(Ldc(string_of_int size));
      JLabel(comp_label);
      JInst(Dup);
      JInst(Ifeq(end_label));
      JInst(Iconst_1);
      JInst(Isub);
      JInst(Dup2);
      JInst(Pop);
      JLabel(body_label) ] @ 
    (type_init_exps t) @ 
    (wrap_type t) @
    [ JInst(InvokeVirtual({
        method_name = flstring jc_list_class "add";
        arg_types = [JRef(jc_object)]; return_type = JBool; }));
      JInst(Pop);
      JInst(Goto(comp_label));
      JLabel(end_label);
      JInst(Pop) ]
| GoSlice _ -> 
    [ JInst(New(jc_list_class));
      JInst(Dup);
      JInst(InvokeSpecial({ 
        method_name = flstring jc_list_class "<init>";
        arg_types = []; return_type = JVoid; })); ] 

| GoFunction _ | NewType _ -> raise (InternalError("You shouldn't have to do initilize these types"))

let get_local_var_decl_mappings next_index mvd_list = 
  let mapping_from_svd svd = 
    let SingleVarDecl(id, _, _) = svd in 
    let _, gotype, var_num = id_info id in 
    let ind = next_index () in 
    let () = (match get_word_size gotype with
    | Two -> ignore (next_index ()) (* It takes up two spots *)
    | One -> ()) in 
    (var_num, (ind, get_jvm_type gotype))
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

(* This function takes a statement and if there is any new local var, gives back a map of 
variable number -> local_id_entry *)
let rec get_mapping_from_stmt prev_map next_index (LinedStatement(_, stmt)) = match stmt with
| VarDeclBlockStatement(mvd_list) -> get_local_var_decl_mappings next_index mvd_list
| ShortVarDeclStatement(shortvd_list) -> 
    let mapping_from_single_shvd (ShortVarDecl(id, _)) = match id with
    | BlankID -> LocalVarMap.empty
    | _ -> 
      let _, gotype, var_num = id_info id in
      if LocalVarMap.mem var_num prev_map then LocalVarMap.empty 
      else 
        let ind = next_index () in 
        let () = (match get_word_size gotype with
          | Two -> ignore (next_index ()) (* It takes up two spots *)
          | One -> ()) in 
        LocalVarMap.add var_num (ind, get_jvm_type gotype) LocalVarMap.empty
    in 
    List.fold_left 
      (fun old_map new_map -> 
        LocalVarMap.merge merge_maps old_map new_map)
      LocalVarMap.empty
      (List.map mapping_from_single_shvd shortvd_list)
| IfStatement(tiny_stmt_op, _, then_stmts, else_stmts_op) -> 
    let init_mapping = match tiny_stmt_op with
    | None -> LocalVarMap.empty
    | Some s -> get_mapping_from_stmt prev_map next_index s 
    in  
    let else_mapping = (match else_stmts_op with
      | None -> LocalVarMap.empty
      | Some (sl) -> 
          List.fold_left 
            (LocalVarMap.merge merge_maps)
            LocalVarMap.empty
            (List.map (get_mapping_from_stmt prev_map next_index) sl)
      )
    in 
    let body_mapping = 
      List.fold_left 
        (LocalVarMap.merge merge_maps)
        else_mapping
        (List.map (get_mapping_from_stmt prev_map next_index) then_stmts)
    in 
    LocalVarMap.merge merge_maps init_mapping body_mapping 
| SwitchStatement(tiny_stmt_op, _, switch_case_list) -> 
    let init_mapping = match tiny_stmt_op with
    | None -> LocalVarMap.empty
    | Some s -> get_mapping_from_stmt prev_map next_index s 
    in 
    let switch_mapping sw = (match sw with 
    | SwitchCase(_, sl) -> 
        List.fold_left 
          (LocalVarMap.merge merge_maps)
          LocalVarMap.empty
          (List.map (get_mapping_from_stmt prev_map next_index) sl)
    | DefaultCase (sl) -> 
        List.fold_left 
          (LocalVarMap.merge merge_maps)
          LocalVarMap.empty
          (List.map (get_mapping_from_stmt prev_map next_index) sl) )
    in 
    let body_mapping = 
      List.fold_left
        (LocalVarMap.merge merge_maps)
        LocalVarMap.empty
        (List.map switch_mapping switch_case_list) in
    LocalVarMap.merge merge_maps init_mapping body_mapping 

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
| BlockStatement(stmt_list) -> 
      List.fold_left 
        (LocalVarMap.merge merge_maps)
        LocalVarMap.empty
        (List.map (get_mapping_from_stmt prev_map next_index) stmt_list)
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
| RuneLit(r) -> 
  let int_r = Char.code(String.get r 0) in 
  [JInst(Ldc(string_of_int int_r))]  
| IntLit(DecInt(s)) -> [JInst(Ldc(s))]
| IntLit(OctalInt(s)) -> 
    let int_repr = int_of_string ("0o" ^ s) in 
    [JInst(Ldc(string_of_int int_repr))]
| IntLit(HexInt(s)) -> 
    let int_repr = int_of_string s in
    [JInst(Ldc(string_of_int int_repr))]
| FloatLit(s) -> [JInst(Ldc2w(s))]
| _ -> raise (InternalError("process_literal not matched"))

(*this function assumes e1 e2 are computed and results are on top of the stack.
It leaves a 0 or 1 on stack depending on the result. *)
let rec compare_expressions t = 
  let label_serial = next_bool_exp_count () in 
  let true_label = "True_" ^ (string_of_int label_serial) in
  let false_label = "False_" ^ (string_of_int label_serial) in 
  let end_label = "EndBoolExp_" ^ (string_of_int label_serial) in
  let true_false_boilerplate = 
    [ JLabel(false_label);
      JInst(Iconst_0);
      JInst(Goto(end_label));
      JLabel(true_label);
      JInst(Iconst_1);
      JLabel(end_label);
    ] in 
  (match t with
   | GoInt | GoRune -> [ JInst(ICmpeq(true_label)) ] @ true_false_boilerplate
   | GoFloat ->
     [ JInst(DCmpg);
      JInst(Ifeq(true_label)); ] @ true_false_boilerplate
   | GoString ->
      [ JInst(InvokeVirtual({
        method_name = jc_equals;
        arg_types = [JRef(jc_object)];
        return_type = JBool; } ) ) ]
   | GoBool -> 
      [JInst(Ixor);
       JInst(Ifeq(true_label));] @ true_false_boilerplate
   | GoArray _ | GoSlice _ -> [JInst(InvokeVirtual( { 
        method_name = flstring jc_list_class "equals";
        arg_types = [JRef(jc_object)]; return_type = JBool; })) ]
   | GoStruct(l) -> [JInst(InvokeVirtual( { 
        method_name = flstring (!scmap l) "equals";
        arg_types = [JRef(jc_object)]; return_type = JBool; })) ]
   | GoCustom(n, t) -> compare_expressions t
   | GoFunction _ | NewType _  -> invalid_arg "This type is not comaparable"  
  )

let rec process_expression exp = 
let Expression(e, t) = exp in match e with 
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
    let call_inst = match fun_type with
    | GoFunction(at, rto) -> process_func_call fun_name arg_expressions at rto
    | NewType(t) -> 
        let exp_inst = (match arg_expressions with
          | [] -> raise (InternalError ("should have a value"))
          | h::[] -> process_expression h
          | h::t -> raise (InternalError ("should only have 1 expression"))
        ) in
        let cast_inst = process_type_cast (base_type fun_type) (base_type t) in
        exp_inst @ cast_inst
    | _ -> raise (InternalError("Only function types can be called. Do we have a typechecker bug?"))
    in call_inst 

| BinaryExp(op, e1, e2) -> process_binary_expression op e1 e2
| UnaryExp(op, e) -> process_unary_expression op e
| SelectExp(e, id) -> 
    (process_expression e) @ 
    [JInst(GetField(
      flstring (struct_cname_of_expression e) (string_of_id id),
       get_jvm_type (exp_type exp) )) ]
| TypeCastExp(ts, origin_exp) -> 
  let e_inst = process_expression origin_exp in
  let target_type = (match ts with
    | BasicType (typ) -> (match typ with
      | IntType -> GoInt
      | FloatType -> GoFloat
      | RuneType -> GoRune
      | _ ->  raise (TypeCastError ("invalid type for type cast. "))
      )
    | _ -> raise (TypeCastError ("invalid type for type cast. "))
  )
  in
  let origin_type = exp_type origin_exp in
  let cast_inst = process_type_cast target_type origin_type in
  e_inst @ cast_inst 
| IndexExp(lexp, inte) ->
    (process_expression lexp) @ (process_expression inte) @ 
    [  JInst(InvokeVirtual({ method_name = flstring jc_list_class "get";
         arg_types = [JInt]; return_type = JRef(jc_object); })) ] @ (unwrap_type (exp_type exp))
| AppendExp(slice_var, new_elm_exp) -> 
    let _, _, var_num = id_info slice_var in 
      [ PS(LoadVar(var_num));
        JInst(InvokeVirtual({ 
              method_name = flstring jc_list_class jc_clone;
              arg_types = []; return_type = JRef(jc_object); }));
        JInst(CheckCast(jc_list_class));
        JInst(Dup); ]
     @ (process_exp_for_assignment new_elm_exp) 
     @ (wrap_type (exp_type new_elm_exp)) @ 
     [ JInst(InvokeVirtual({
        method_name = flstring jc_list_class "add";
        arg_types = [JRef(jc_object)]; return_type = JBool; })); 
       JInst(Pop)]



(*helper function to process func call in process_expression, only meant to be called from there.*)
and process_func_call fun_name arg_expressions arg_gotypes ret_gotypeop = 
    let jfunction_sig = 
      { method_name = main_class_name ^ "/" ^ fun_name;
        arg_types = List.map get_jvm_type arg_gotypes;
        return_type = match ret_gotypeop with
        | None -> JVoid 
        | Some t -> get_jvm_type t ;
      } in 
    let arg_load_instructions = List.map process_exp_for_assignment arg_expressions in
    let stack_null_instructions = (match ret_gotypeop with
      | None -> [JInst(AConstNull)]
      | Some _ -> [])
    in  
    (List.flatten arg_load_instructions) @ 
    [JInst(InvokeStatic(jfunction_sig))] @
    stack_null_instructions

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
  match base_type (exp_type e1) with
  | GoInt 
  | GoRune -> e1_insts @ e2_insts @ (process_binary_int_expr op true_label true_false_boilerplate)
  | GoFloat -> e1_insts @ e2_insts @ (process_binary_float_expr op true_label true_false_boilerplate)
  | GoBool -> e1_insts @ e2_insts @ (process_binary_bool_expr op true_label true2_label 
                                      false_label false2_label end_label true_false_boilerplate)
  | GoString -> process_binary_string_expr op (exp_type e1) e1_insts e2_insts true_label true_false_boilerplate
  | (GoArray _) as t -> e1_insts @ e2_insts @
    (match op with
      | BinEq -> compare_expressions t
      | BinNotEq -> (compare_expressions t) @ (negate_boolean ())
      | _ -> raise (InternalError("This binary op not valid for arrays"))
    )
  | (GoSlice _) as t -> e1_insts @ e2_insts @
    (match op with
      | BinEq -> compare_expressions t
      | BinNotEq -> (compare_expressions t) @ (negate_boolean ())
      | _ -> raise (InternalError("This binary op not valid for slices"))
    )
  | GoStruct(fl) as t -> e1_insts @ e2_insts @
    (match op with
      | BinEq -> compare_expressions t
      | BinNotEq -> (compare_expressions t) @ (negate_boolean ())
      | _ -> raise (InternalError("This binary op not valid for structs"))
    )
  | GoCustom _ -> raise (InternalError("To handle custom types, call base_type on the type."))
  | GoFunction _ | NewType _  -> raise (InternalError("These types cannot be used in binary expressions."))

(*BinEq is not extracted to compare_expressions before it requires typ argument.
and also because instructions are very short. *)
and process_binary_int_expr op true_label true_false_boilerplate = (match op with
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
      | BinShiftLeft -> [JInst(Ishl)] (* some rune will cause overflow*)
      | BinShiftRight -> [JInst(Ishr);] (* some rune will cause overflow*)
      | BinBitAnd -> [JInst(Iand);]
      | BinBitAndNot -> 
        [JInst(Iconst_m1);
         JInst(Ixor);
         JInst(Iand);
        ]
      | BinOr|BinAnd-> raise (InternalError("int type is not valid for or and and. Typecheck fail"))
    )

(*BinEq is not extracted to compare_expressions before it requires typ argument.
and also because instructions are very short. *)
and process_binary_float_expr op true_label true_false_boilerplate = (match op with
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
      | BinOr|BinAnd| BinBitAndNot -> raise (InternalError("float type is not valid for this binary operation. Typecheck fail"))
    )
and process_binary_bool_expr op true_label true2_label false_label false2_label end_label true_false_boilerplate =
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
      | _ -> raise (InternalError("bool type is not valid for this operation. Typecheck fail"))
    )
(* typ argument is added so it can use compare_expression function *)
and process_binary_string_expr op typ e1_insts e2_insts true_label true_false_boilerplate = 
    let invoke_comp = 
        [ JInst(InvokeVirtual({
          method_name = jc_compare;
          arg_types = [JRef(jc_string)];
          return_type = JInt; } ) );
          JInst(Iconst_0); ] 
    in
    let invoke_str_builder = 
        [ JInst(InvokeVirtual({
          method_name = jc_append;
          arg_types = [JRef(jc_string)];
          return_type = JRef(jc_string_build); } ) );
       ] 
    in
    (match op with 
      | BinEq -> e1_insts @ e2_insts @
        [ JInst(InvokeVirtual({
          method_name = jc_equals;
          arg_types = [JRef(jc_object)];
          return_type = JBool; } ) ) ]
      | BinNotEq -> e1_insts @ e2_insts @
        [ JInst(InvokeVirtual({
          method_name = jc_equals;
          arg_types = [JRef(jc_object)];
          return_type = JBool; } ) ) ]
        @ [JInst(Ifeq(true_label));]
        @ true_false_boilerplate
      | BinLess -> e1_insts @ e2_insts 
        @ invoke_comp
        @ [ JInst(ICmplt(true_label));]
        @ true_false_boilerplate
      | BinLessEq -> e1_insts @ e2_insts 
        @ invoke_comp
        @ [ JInst(ICmple(true_label));]
        @ true_false_boilerplate
      | BinGreater -> e1_insts @ e2_insts 
        @ invoke_comp
        @ [JInst(ICmpgt(true_label));]
        @ true_false_boilerplate
      | BinGreaterEq -> e1_insts @ e2_insts 
        @ invoke_comp
        @ [ JInst(ICmpge(true_label));]
        @ true_false_boilerplate
      | BinPlus -> 
          [ JInst(New(jc_string_build));
            JInst(Dup);
            JInst(InvokeSpecial({
            method_name = jc_sb_init;
            arg_types = [];
            return_type = JVoid; } ) );]
        @   e1_insts 
        @   invoke_str_builder 
        @   e2_insts 
        @   invoke_str_builder
        @ [ JInst(InvokeVirtual({
            method_name = jc_sb_toString;
            arg_types = [];
            return_type = JRef(jc_string); } ) );
          ]
      | _ -> raise (InternalError("Not a valid string operation. Typecheck fail")) (*not needed*)
    )

and process_unary_expression op e = 
  let e_insts = process_expression e in 
  let label_serial = next_bool_exp_count () in 
  let true_label = "True_" ^ (string_of_int label_serial) in
  let false_label = "False_" ^ (string_of_int label_serial) in 
  let end_label = "EndBoolExp_" ^ (string_of_int label_serial) in
  let true_false_boilerplate = 
    [ JLabel(false_label);
      JInst(Iconst_0);
      JInst(Goto(end_label));
      JLabel(true_label);
      JInst(Iconst_1);
      JLabel(end_label);
    ] in 
  e_insts @
  match base_type (exp_type e) with 
  | GoInt ->
    (match op with 
    | UPlus -> []
    | UMinus -> [JInst(Ineg);]
    | UNot -> raise (InternalError("Not a valid int unary op. Typecheck fail"))
    | UCaret -> [JInst(Iconst_m1);JInst(Ixor);]
    )
  | GoFloat ->
    (match op with 
    | UPlus -> []
    | UMinus -> [JInst(Dneg);]
    | UNot | UCaret -> raise (InternalError("Not a valid float64 unary op. Typecheck fail"))
    )
  | GoRune ->
    (match op with 
    | UPlus -> []
    | UMinus -> [JInst(Ineg);]
    | UNot -> raise (InternalError("Not a valid rune unary op. Typecheck fail"))
    | UCaret -> [JInst(Iconst_m1);JInst(Ixor);]
    )
  | GoBool ->
    (match op with  
    | UNot -> [JInst(Ifeq(true_label));] @ true_false_boilerplate
    | UPlus | UMinus | UCaret -> raise (InternalError("Not a valid rune unary op. Typecheck fail"))
    )
  | GoString | GoArray _ | GoSlice _ | GoStruct _ | GoFunction _ | NewType _ 
       -> raise (InternalError("No unary operatino supported on this type"))
  | GoCustom _ -> raise (InternalError "Call base_type to handle custom type")

(* Assume target type and origin type are GoType reduced to base type.
this function supports more types then types allowed in type_cast_expressions 
in order to support type cast with custom types. *)
and process_type_cast target_t origin_t = (match target_t with
  | GoInt -> (match origin_t with
      | GoRune | GoInt -> []
      | _ -> raise (TypeCastError ("Cannot type cast to int "))
    )
  | GoFloat -> (match origin_t with
      | GoFloat -> []
      | GoInt -> [JInst(I2d)]
      | GoRune -> [JInst(I2d)]
      | _ -> raise (TypeCastError ("Cannot type cast to float "))
    )
  | GoRune -> (match origin_t with
      | GoInt | GoRune -> []
      | _ -> raise (TypeCastError ("Cannot type cast to rune "))
    )
  | GoBool -> (match origin_t with 
      | GoBool -> []
      | GoInt -> raise (TypeCastError ("Cannot type cast from int to boolean. "))
      | _ -> raise (TypeCastError ("Cannot type cast to boolean "))
  )
  | GoArray _ | GoSlice _ | GoStruct _ | GoFunction _ | NewType _
      -> raise (InternalError("Typecast is only supported for basic types. Typecheck fail"))
  | GoString -> raise (InternalError("String typecast not supported. Typecheck fail"))
  | GoCustom _ -> invalid_arg "Reduce to base type before processing type cast"
)

and process_exp_for_assignment e = 
  let exp_eval = process_expression e in 
  let jt = get_jvm_type (exp_type e) in 
  let clone_instructions = match jt with
    | JRef(class_name) ->
        if class_name = jc_string then [] else 
        if ((function GoSlice _ -> true | _ -> false) (exp_type e)) then [] else (* Slices are passed by reference *)
          [ JInst(InvokeVirtual({ 
              method_name = flstring class_name jc_clone;
              arg_types = []; return_type = JRef(jc_object); }));
            JInst(CheckCast(class_name)); ]
    | JVoid | JInt | JDouble | JBool -> []
    | JArray _ -> raise (InternalError("JArray should not be used for any go data structure. Use GoLiteList instead."))
  in
  exp_eval @ clone_instructions

let process_global_var_decl mvd_list =
  let mapping_from_svd svd = 
    let SingleVarDecl(id, tp_op, exp_op) = svd in
    let name, gotype, var_num = id_info id in
    let init_code = match exp_op with
    | None -> (type_init_exps gotype) @ [PS(StoreVar(var_num))]
    | Some(e) -> (process_exp_for_assignment e) @ [PS(StoreVar(var_num))] in 
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
    let _, gotype, var_num = id_info id in 
    match exp_op with
    | None -> (type_init_exps gotype) @ [PS(StoreVar(var_num))]
    | Some e -> (process_exp_for_assignment e) @ [PS(StoreVar(var_num))]
  in 
  let insts_from_mvd mvd = 
    let MultipleVarDecl(svd_list) = mvd in 
    List.flatten (List.map insts_from_svd svd_list)
  in
  List.flatten (List.map insts_from_mvd mvd_list)

let print_single_expression print_method_name e = 
  JInst(GetStatic(jc_sysout, JRef(jc_printstream))) :: 
  (match base_type (exp_type e) with
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


let rec dummy_pop_instruction_of_type gotype = match gotype with
| GoInt | GoBool | GoRune | GoString | GoStruct _
| GoArray _ | GoSlice _ -> [JInst(Pop)]
| GoFloat -> [JInst(Pop2)]
| GoCustom(s, t) -> dummy_pop_instruction_of_type t
| NewType _ -> raise (InternalError("What are you even trying to do here"))
| GoFunction(_, ret_op) -> raise (InternalError("Um not you can't leave a function on stack"))

let is_exp_void_fun_call exp = 
  let Expression(e, t) = exp in 
  match !t with 
  | Some _ -> false 
  | None -> match e with 
      | FunctionCallExp(fun_exp, _) -> (match exp_type fun_exp with 
        | GoFunction(_, None) -> true
        | GoFunction(_, Some _) -> false 
        | _ -> raise (InternalError("Typechecker - you had one job.")) )
      | _ -> raise (InternalError("Life is so void of meaning."))

let rec process_statement bl cl (LinedStatement(_, s)) = match s with
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
| ExpressionStatement(e) -> 
    let exp_insts =  process_expression(e) in 
    if is_exp_void_fun_call e then exp_insts @ [JInst(Pop)]
    else exp_insts @ (dummy_pop_instruction_of_type (exp_type e))
| EmptyStatement -> []
| BlockStatement(stmt_list) -> List.flatten (List.map (process_statement bl cl) stmt_list)
| ReturnStatement(e_op) -> (match e_op with
    | None -> [JInst(Return)];
    | Some e -> (process_expression e) @ (match get_jvm_type (exp_type e) with
      | JVoid -> raise (InternalError("Void expressions cannot be returned"))
      | JInt -> [JInst(IReturn)]
      | JDouble -> [JInst(DReturn)]
      | JBool -> [JInst(IReturn)]
      | JRef _ -> [JInst(AReturn)]
      | JArray _ -> [JInst(AReturn)] ) )

| ForStatement(init_stmt_op, loop_cond_op, post_stmt_op, stmt_list) -> 
    let count = string_of_int (next_loop_count ()) in 
    let loop_check_label = "LoopCheck_" ^ count in 
    let loop_begin_label = "LoopBegin_" ^ count in
    let loop_post_label = "LoopPost_" ^ count in 
    let loop_end_label = "LoopEnd_" ^ count in   
    let init_instructions = match init_stmt_op with
    | None -> []
    | Some s -> process_statement bl cl s in 
    let post_instructions = match post_stmt_op with
    | None -> []
    | Some s -> process_statement bl cl s in 
    let cond_statements = match loop_cond_op with
    | None -> [ JInst(Iconst_1) ]  (* This while true block *)
    | Some(e) -> process_expression e in 
    let body_instructions = 
      List.flatten 
        (List.map 
          (process_statement (Some(loop_end_label)) (Some(loop_post_label)))
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
      JLabel(loop_end_label); JInst(Nop) ]
| ShortVarDeclStatement(shortvd_list) -> 
    (* first evaluate all the arguments, then store them *)
    let exps = List.map (fun (ShortVarDecl(id, e)) -> e) shortvd_list in 
    let vars = List.map (fun (ShortVarDecl(id, e)) -> id) shortvd_list in 
    let exp_instructions = List.flatten (List.map process_exp_for_assignment exps) in 
    let store_instructions = 
      List.map
        (fun id -> match id with 
        | BlankID -> JInst(Pop)
        | id -> let _, _, var_num = id_info id in PS(StoreVar(var_num)) )
        (List.rev vars)
    in
    exp_instructions @ store_instructions
| AssignmentStatement(ass_list) -> 
    let exps = List.map (fun (e1, e2) -> e2) ass_list in 
    let exp_instructions = List.flatten (List.map process_exp_for_assignment exps) in
    
    let single_store_instruction (Expression(e, _)) rexp = match e with
      | IdExp(BlankID) -> [JInst(Pop)]
      | IdExp(id) -> 
          let _, _, var_num = id_info id in 
          [PS(StoreVar(var_num))]
      | IndexExp(e, inte) -> 
          let lvalue_load_inst = process_expression e in 
          let index_load_inst = process_expression inte in 
          let dup_inst = (match get_word_size (exp_type rexp) with 
                  | One -> [JInst(Dup2_x1);]
                  | Two -> [JInst(Dup2_x2);] ) in
          lvalue_load_inst @ index_load_inst @ dup_inst @ 
          [ JInst(Pop2); ] @ (wrap_type (exp_type rexp)) @
          [ JInst(InvokeVirtual({
             method_name = flstring jc_list_class "set";
             arg_types = [JInt; JRef(jc_object)]; return_type = JRef(jc_object);}));
            JInst(Pop); ]
      | SelectExp(lexp, id) -> 
          let cname = struct_cname_of_expression lexp in
          let dup_inst = (match get_word_size (exp_type rexp) with 
                  | One -> [JInst(Dup_x1);]
                  | Two -> [JInst(Dup_x2);] ) in 
          (process_expression lexp) 
          @ dup_inst @ [JInst(Pop); JInst(PutField(flstring cname (string_of_id id), get_jvm_type (exp_type rexp) ))]
      | FunctionCallExp _ 
      | AppendExp _ | TypeCastExp _ | LiteralExp _ 
      | UnaryExp _ | BinaryExp _  -> raise (InternalError("This is not a valid lvalue"))
    in
    let store_instructions = 
      List.flatten 
        (List.map (fun (l, r) -> single_store_instruction l r)  (List.rev ass_list))
    in
    exp_instructions @ store_instructions
| BreakStatement -> ( match bl with
  | None -> raise (InternalError("I know not whence to break"))
  | Some l -> [ JInst(Goto(l)) ] )
| ContinueStatement -> ( match cl with
  | None -> raise (InternalError("I know not whence to continue"))
  | Some l -> [ JInst(Goto(l)) ] )
| IfStatement(init_stmt_op, expr_cond, then_list, else_list_op) ->
    let count = string_of_int (if_count ()) in
    let end_label = "EndIf_" ^ count in 
    let else_label = "Else_" ^ count in
    let init_inst = match init_stmt_op with
    | None -> []
    | Some s -> process_statement bl cl s in 
    let exp_inst = process_expression expr_cond in
    let then_inst = List.flatten (List.map (process_statement bl cl) then_list) in
    let else_inst = (match else_list_op with
      | None -> []
      | Some (sl) -> List.flatten (List.map (process_statement bl cl) sl) )
    in
    init_inst @ exp_inst @
    [JInst(Ifeq(else_label))] @
    then_inst @
    [JInst(Goto(end_label));
     JLabel(else_label)] @
    else_inst @
    [JLabel(end_label); JInst(Nop)]
| SwitchStatement (stmt_op, exp, case_list) -> 
    let init_inst = (match stmt_op with
    | None -> []
    | Some s -> (process_statement bl cl) s) in 
    let case_inst = process_case_list bl cl exp case_list in 
    init_inst @ case_inst 


and process_case_list bl cl switch_exp case_list = 
  let count = string_of_int (switch_count ()) in
  let end_label = "EndSwitch_" ^ count in 
(*   let case_label = "Case_" ^ count in  *)
  let default_label = "Default_" ^ count in 
  (*func to pass to List.find *)
  let get_default case = (match case with
    | SwitchCase(exp_list, sl) -> false
    | DefaultCase(sl) -> true
    )
  in 
  let get_default_inst df = (match df with
    | DefaultCase(sl) -> let stmt_list_inst = List.flatten (List.map (process_statement bl cl) sl) in
      [ JLabel(default_label)] @ stmt_list_inst @
      [ JInst(Goto(end_label))]
    | _ -> raise (InternalError ("shouldn't get here"))
  )
  in
  let default_inst = get_default_inst (List.find get_default case_list) in
  let switch_exp_inst = process_expression switch_exp in
  let type_of_switch_exp = exp_type switch_exp in
  let dup_inst = (match get_word_size type_of_switch_exp with 
    | One -> [JInst(Dup);]
    | Two -> [JInst(Dup2);] ) in
  let pop_inst = (match get_word_size type_of_switch_exp with 
    | One -> [JInst(Pop);]
    | Two -> [JInst(Pop2);] ) in

  (*generate instructions for <case 0, 1, 2 : label> since all these cases leads to the same label(stmt_list) 
    returns a tuple of (case_eval_instruction, corresponding stmt list instruction) *)  
  let process_case c_label one_case = (match one_case with 
  | SwitchCase(exp_list, sl) -> 
  (*takes 1 exp from one case and generate code for that exp compared to switch expr. *)
    let get_one_case_inst exp =
      let typ = exp_type exp in 
      let case_exp_insts = process_expression exp in
      (* e1 @ e2 so their results are on top of the stack*)
      dup_inst @ case_exp_insts @
      compare_expressions typ @
      [JInst(Ifne(c_label))] 
    in
    let evaluation_inst = List.flatten (List.map get_one_case_inst exp_list) in
    let label_stmt_inst = 
      [JLabel(c_label)] @
      List.flatten (List.map (process_statement bl cl) sl) @ [JInst(Goto(end_label))] in
    evaluation_inst, label_stmt_inst
  | _ -> ([JInst(Nop)] , [JInst(Nop)]) (* don't need to generate anything for default case here.*)
  )
  in (*need to increment case_label for each element. *)
  let tuple_list_inst = 
      List.map (fun x -> process_case 
        ("Case_" ^ (string_of_int (switch_count ())))
        x ) case_list
  in 
  let case_list_inst = 
    List.flatten (List.map (fun (a, _) -> a) tuple_list_inst)
  in
  let stmt_list_inst = 
    List.flatten (List.map (fun (_, b) -> b) tuple_list_inst)
  in
  switch_exp_inst @ case_list_inst @ default_inst @ 
  stmt_list_inst @ pop_inst @ 
  [JLabel(end_label)]

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
            let ind = next_index () in 
            let () = (match get_word_size gt with
            | Two -> ignore (next_index ()) (* It takes up two spots *)
            | One -> ()) in 
            LocalVarMap.add var_num (ind, get_jvm_type gt) old_map)
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
  let stmt_code = List.flatten (List.map (process_statement None None) stmt_list) in
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
    class_name = main_class_name;
    source = go_filename;
    top_level_vars = List.rev rev_tlvars; 
    methods = List.rev rev_methods;
  }
