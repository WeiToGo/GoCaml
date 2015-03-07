open Ast
open Printf
open Symtable

(* globals *)

let out_channel = stdout
let err_channel = stderr

(* --~~~~~~--*** Exceptions ***--~~~~~~-- *)


exception NotImplemented
exception TypeCheckError of string
exception VariableRedeclaration of string
exception InternalError of string
exception Abort

(* Statement type checking errors come bundled with line number *)
exception StmtTypeCheckError of (string * int)

let prev_decl_msg scope id = match id with
| BlankID -> raise (InternalError "BlankID is never defined before")
| ID(name, _) -> 
  let entry = lookup_current scope name in
  let Entry(_, _, _, ln) = entry in
  "Previous declaration of " ^ name ^ " at line " ^
  (string_of_int ln) ^ "."

let assign_error_msg left_type right_type = 
  "Trying to assign value to type " ^ (string_of_type right_type) ^
  " to a vairable of type " ^ (string_of_type left_type)

(* --~~~~~~--*** Helper Functions ***--~~~~~~-- *)

let type_of_entry entry =
  let Entry(_, typ, _, _) = entry in 
  typ 

let string_of_id id = match id with
| BlankID -> "BlankID"
| ID(name, _) -> name

(* Adds the symbol for an identifier to scope, and also updates
 * the ref field of id to point to the symbol table entry.
 * Silently ignores attempts to add blank id *)
let add_id scope id typ ln = match id with
  | BlankID -> ()
  | ID(name, sym_ref) -> 
      let entry = add_sym scope name typ ln in
      sym_ref := Some(entry)


(* Looks up id in scope. If found, updates the ref field of id to
 * point to the symbol table entry.
 * If BlankID has been properly weeded out, you should never have
 * to do a lookup with BlankID *)
let lookup_id scope id = match id with
| BlankID -> raise (InternalError "Attempted lookup of BlankID in symbol table")
| ID(name, sym_ref) -> 
      let entry = lookup scope name in
      sym_ref := Some(entry); type_of_entry entry

(* Returns whether ID is defined in current scope.
 * BlankID is never defined in current scope. *)
let id_in_current_scope scope id = match id with
| BlankID -> false
| ID(name, sym_ref) -> in_current_scope scope name

let int_of_int_lit lit = match lit with 
  | DecInt(s) -> int_of_string s
  | HexInt(s) -> int_of_string s  (* OCaml automatically processes hex strings *)
  | OctalInt(s) -> int_of_string ("0o" ^ s) 


(* Converts an AST type_spec to a gotype defined for symbol table *)
let rec gotype_of_typspec ctx tspec = match tspec with
  | BasicType(bt) -> (
      match bt with
      | IntType -> GoInt
      | FloatType -> GoFloat
      | BoolType -> GoBool
      | RuneType -> GoRune
      | StringType -> GoString
  )
  | SliceType(t) -> GoSlice(gotype_of_typspec ctx t)
  | ArrayType(int_lit, t) -> GoArray(int_of_int_lit int_lit, gotype_of_typspec ctx t)
  | StructType(msfd_list) -> GoStruct(map_of_struct_fields ctx msfd_list)
  | FunctionType(ts_list, ts_ret) -> (
      let args = List.map (gotype_of_typspec ctx) ts_list in
      match ts_ret with
      | None -> GoFunction(args, None)
      | Some(t) -> GoFunction(args, Some(gotype_of_typspec ctx t))
  )
  | CustomType(id) -> ( match id with
    | BlankID -> raise (TypeCheckError "Blank ID cannot be used as a type")
    | ID(name, _) -> (
        let declared_type = (
          try lookup_id ctx id
          with Not_found -> raise (TypeCheckError (name ^ " was not previously declared as a type"))
        )
        in
        match declared_type with
        | NewType gt -> GoCustom (name, gt)
        | _ -> raise (TypeCheckError (name ^ " was not declared as a type."))
    )
  )

(* Converts a multi struct field declaration list into a StructField map *)
and map_of_struct_fields ctx msfd_list = 
  let struct_map_merge key a b = 
    (* Function to merge to two struct field maps *)
    match a,b with
    | None, None -> None
    | Some p, None -> Some p
    | None, Some q -> Some q
    | Some p, Some q -> raise (TypeCheckError "Duplicate field declaration in struct")
  in
  let map_of_msfd = function
  | MultipleStructFieldDecl(ssfd_list) -> 
      let singleton_svd_map svd = 
        let SingleStructFieldDecl(identifier, type_spec) = svd in
        ( match identifier with 
          | BlankID -> raise (TypeCheckError "Blank ID not allowed as struct field")
          | ID(name, _) -> StructFields.singleton name (gotype_of_typspec ctx type_spec) 
        )
      in
      let single_maps_list = List.map singleton_svd_map ssfd_list in
      List.fold_left 
        (fun x y -> StructFields.merge struct_map_merge x y)
        StructFields.empty single_maps_list
  in
  let multi_maps_list = List.map map_of_msfd msfd_list in
  List.fold_left 
    (fun x y -> StructFields.merge struct_map_merge x y) 
    StructFields.empty multi_maps_list


(* resolve_to_base (GoCustom("foo", GoInt)) -> GoInt 
 * resolve_to_base (GoArray (42, GoCustom("foo", GoInt))) -> GoArray (42, GoInt)
 * resolve_to_base GoRune -> GoRune  *)
let rec resolve_to_base typ = match typ with
| GoInt | GoFloat | GoBool | GoRune | GoString -> typ
| GoSlice t -> GoSlice (resolve_to_base t)
| GoArray(size, t) -> GoArray(size, resolve_to_base t)
| GoStruct(flds) -> GoStruct (StructFields.map resolve_to_base flds)
| GoFunction(args, ret) -> 
  ( match ret with
  | None -> GoFunction (List.map resolve_to_base args, None)
  | Some(t) -> GoFunction (List.map resolve_to_base args, Some(resolve_to_base t)) )
| GoCustom(name, t) -> resolve_to_base t
| NewType(t) -> NewType (resolve_to_base t)

(* 
let rec resolve_type_declaration typ = match type with
| patt -> expr
| _ -> expr2
(* We should have *)
let rec  *)


let rec is_comparable gtype = match gtype with
| GoBool | GoFloat | GoInt | GoString | GoRune -> true
| GoSlice(_) | GoFunction(_) | NewType(_) -> false
| GoArray(_, t) -> is_comparable t
| GoStruct(fields) -> StructFields.for_all (fun name typ -> is_comparable typ) fields
| GoCustom(_, t) -> is_comparable (resolve_to_base t)

let rec is_ordered gtype = match gtype with
| GoInt | GoFloat | GoString | GoRune -> true
| GoArray(_) | GoSlice(_) | GoStruct(_) 
| GoBool | GoFunction(_) | NewType(_) -> false
| GoCustom(name, t) -> is_ordered (resolve_to_base t)

let rec is_integer gtype = match gtype with
| GoInt | GoRune -> true
| GoArray(_) | GoSlice(_) | GoString | GoBool 
| GoStruct(_) | GoFloat | GoFunction(_) | NewType(_) -> false
| GoCustom(name, t) -> is_integer (resolve_to_base t)

let rec is_numeric gtype = match gtype with
| GoInt | GoFloat | GoRune  -> true
| GoString | GoBool
| GoFunction(_) | GoSlice(_) | GoArray(_) | NewType(_) | GoStruct(_) -> false
| GoCustom(name, t) -> is_numeric (resolve_to_base t)

let rec is_num_string gtype = match is_numeric gtype, gtype with
| true, _ -> true
| false, GoString -> true
| false, GoCustom(name, t) -> is_num_string (resolve_to_base t)
| false, _ -> false

let is_exp_an_id = function
| IdExp(_) -> true
| _ -> false

let id_from_exp = function
| IdExp(id) -> id
| _ -> raise (InternalError "Tried to extract ID from non-id expression")




let merge_type t1 t2 = 
  if t1 = t2 then t1
  else raise (TypeCheckError 
              ( "Unequal types in binary expressions:  " ^ (string_of_type t1) ^
                " and " ^ (string_of_type t2) ) ) 

let rec get_expression_type ctx e = match e with
| IdExp(id) -> 
  ( try lookup_id ctx id
    with Not_found -> 
      raise (TypeCheckError ("Undeclared identifier " ^ (string_of_id id)) ) )
| LiteralExp(lit_exp) ->
  ( match lit_exp with
  | IntLit _ -> GoInt
  | FloatLit _ -> GoFloat
  | RuneLit _ -> GoRune
  | StringLit _ -> GoString
  | RawStringLit _ -> GoString )
| UnaryExp(op, exp) ->
  ( match op with
    | UPlus | UMinus -> 
      let exp_type = get_expression_type ctx exp in
      ( match resolve_to_base exp_type with
        | GoInt | GoFloat | GoRune -> exp_type
        | _ -> raise (TypeCheckError 
                      "Operand of arithmatic unary operand must be of type int, float, or rune") )
    | UNot -> 
        let exp_type = get_expression_type ctx exp in
        ( match resolve_to_base exp_type with
        | GoBool -> exp_type
        | _ -> raise (TypeCheckError
                      "Operand of unary not must be of type bool") )
    | UCaret -> 
        let exp_type = get_expression_type ctx exp in
        ( match resolve_to_base exp_type with
        | GoInt | GoRune -> exp_type
        | _ -> raise (TypeCheckError
                      "Operand of bitwise negation must be of type int or rune") )
  )

| BinaryExp(op, e1, e2) -> 
  let e1_type = (get_expression_type ctx e1) in
  let e2_type = (get_expression_type ctx e2) in
  let get_bin_exp_type property_func property_name = 
    ( match property_func e1_type, property_func e2_type with
      | true, true -> merge_type e1_type e2_type
      | true, _ -> 
          raise (TypeCheckError
            ( (string_of_type e2_type) ^ " is not " ^ property_name ^ "." ) )
      | _, _ -> 
          raise (TypeCheckError
            ( (string_of_type e1_type) ^ " is not " ^ property_name ^ "." ) ) )
  in
  ( match op with
    | BinOr | BinAnd -> 
        get_bin_exp_type (fun x -> x == GoBool) "of type boolean" 
    | BinEq | BinNotEq -> 
        get_bin_exp_type is_comparable "comparable"
    | BinLess | BinLessEq | BinGreater | BinGreaterEq ->
        get_bin_exp_type is_ordered "ordered"
    | BinPlus -> 
        get_bin_exp_type is_num_string "numeric or string"
    | BinMinus | BinMult | BinDiv | BinMod ->
        get_bin_exp_type is_numeric "numeric"
    | BinBitOr | BinBitAnd | BinShiftLeft | BinShiftRight 
    | BinBitAndNot | BinBitXor -> 
        get_bin_exp_type is_numeric "numeric"
  )     
| FunctionCallExp (caller, args_list) -> ( 
    (* Careful - this can be a type cast expression *)
    match caller with  
    | IdExp(BlankID) -> raise (TypeCheckError "Trying to read from BlankID") 
    | IdExp(id) -> 
      ( try 
        ( match (lookup_id ctx id) with 
          | NewType(_) as t -> 
              let exp_to_cast = (match args_list with
              | [] -> raise (TypeCheckError "Typecast statement with empty argument")
              | a :: b :: t -> raise (TypeCheckError "Typecast statement with more that one argument")
              | h :: [] -> h ) 
              in
              get_type_cast_exp_type ctx (Some id) t exp_to_cast
          | GoFunction(arg_types, ret)-> function_call_type ctx arg_types args_list ret
          | GoCustom(_) -> raise (InternalError "You should resolve custom types before matching")
          | _ -> raise (TypeCheckError ((string_of_id id) ^ "is not a function") ) )
       with Not_found -> raise (TypeCheckError ("Functino " ^ (string_of_id id) ^ " not defined.")) )
    | _ as exp -> ( match get_expression_type ctx exp with
        | GoFunction(arg_types, ret) -> function_call_type ctx arg_types args_list ret
        | _ -> raise (TypeCheckError "Only function expressions can be called.") )
    )

| AppendExp(id, exp) -> ( match (get_expression_type ctx (IdExp id)) with
    | GoSlice(t) as gst ->
      ( let exp_type = get_expression_type ctx exp in
        if (get_expression_type ctx exp = t) then t
        else raise (TypeCheckError ("Cannot append expression of type " ^ (string_of_type exp_type) ^ 
              " to slice of type " ^ (string_of_type gst ) ) ) ) 
    | _ -> raise (TypeCheckError ((string_of_id id) ^ " is not of type slice")) )

| IndexExp(lexp, ind_exp) -> 
    let () = ( match get_expression_type ctx ind_exp with
    | GoInt -> ()
    | _ -> raise (TypeCheckError "Array index must be an integer") )
    in
    ( match (get_expression_type ctx lexp) with
      | GoArray(_, t) -> t
      | GoSlice(t) -> t
      | _ -> raise (TypeCheckError "You can only index into arrays or structs")
    ) 


| SelectExp(exp, id) -> ( match (get_expression_type ctx exp) with 
    | GoStruct(fields) -> (
        try StructFields.find (string_of_id id) fields
        with Not_found -> 
          raise (TypeCheckError ("This struct has no field named " ^
              (string_of_id id) ) ) )
    | _ -> raise (TypeCheckError "Trying to access field in non-struct expresison") )

| TypeCastExp(ts, exp) -> get_type_cast_exp_type ctx None (gotype_of_typspec ctx ts) exp

and function_call_type ctx arg_types args_list ret = 
  let rec aux l1 l2 = ( match l1, l2 with
  | a :: s, b :: t -> 
      let b_type = get_expression_type ctx b in
      if (a == b_type) then aux s t
      else raise (TypeCheckError ("Argument expected of type " ^ (string_of_type a) ^
        "  but argument received of type " ^ (string_of_type b_type)))

  | [], [] -> () 
  | _, _ -> raise (TypeCheckError "Function called with wrong number of arguments") )(*TODO: print how many *)
  in
  let () = aux arg_types args_list
  in
  ( match ret with
  | None -> raise (TypeCheckError "Void function call used as a value")
  | Some(t) -> t )

and get_type_cast_exp_type ctx id_op typ exp = 
  let exp_type = get_expression_type ctx exp in
  ( match (resolve_to_base exp_type) with
    | GoInt | GoFloat | GoBool | GoRune 
    | NewType(GoInt) | NewType(GoFloat)
    | NewType(GoBool) | NewType(GoRune) -> 
      ( match (resolve_to_base typ) with
        | GoInt | GoFloat | GoBool | GoRune -> typ
        | NewType(t) -> (match (resolve_to_base t) with
            | GoInt | GoFloat | GoBool | GoRune -> (match id_op with 
                | None -> raise (InternalError "No custom type id supplied")
                | Some(id) -> GoCustom((string_of_id id), t) )
            | _ -> raise (TypeCheckError ("Casting to type " ^ (string_of_type t) ^ " is not supported")) )
        | _ -> raise (TypeCheckError ("Casting to type " ^ (string_of_type typ) ^ " is not supported")) )
    | _ -> raise (TypeCheckError ("Casting types of type "  ^ (string_of_type exp_type) ^ " is not supported")) )


let gotype_of_function_sig ctx fsig = 
  let FunctionSig(args_list, ret) = fsig in
  let arg_types = List.map (fun (FunctionArg (id, ts)) -> gotype_of_typspec ctx ts) args_list in
  let ret_type = (match ret with
      | None -> None
      | Some(t) -> Some (gotype_of_typspec ctx t) )
  in
  GoFunction (arg_types, ret_type)

 
let is_void_function_call ctx exp = match exp with
| FunctionCallExp(IdExp(id), args) -> 
  ( match lookup_id ctx id with
    | GoFunction(arg_types, None) -> true
    | _ -> false )
| _ -> false


(* --~~~~~~--*** The Type Checker ***--~~~~~~-- *)


let rec tc_program ctx = function
  | Program(pkg_decl, top_lined_dc_list) ->
    begin
       tc_package_decl ctx pkg_decl;
       List.iter (tc_lined_top_decl ctx) top_lined_dc_list
    end 

and tc_package_decl ctx = function
  | Package(_) -> () (* Nothing to typecheck *)

and tc_lined_top_decl ctx = function
  | LinedTD(tdcl, ln) -> 
      try tc_top_decl ln ctx tdcl
      with 
      | TypeCheckError s ->
        fprintf err_channel "Typing Error at line %d:\n" ln;
        fprintf err_channel "%s\n" s;
        raise Abort
      | StmtTypeCheckError (s, ln) ->
        fprintf err_channel "Typing Error at line %d:\n" ln;
        fprintf err_channel "%s\n" s;
        raise Abort

and tc_top_decl ln ctx = function
  | FunctionDecl(id, fsig, body) -> 
      if (id_in_current_scope ctx id) then 
        raise (VariableRedeclaration (prev_decl_msg ctx id))
      else
        let typ = gotype_of_function_sig ctx fsig in

        let () = add_id ctx id typ ln in
        let fun_ret_typ = function
            | GoFunction(_, ret_type) -> ret_type
            | _ -> raise (InternalError "Type GoFunction expected")
        in
        let ret_type = fun_ret_typ typ in
        let body_scope = open_scope ctx in  
        let FunctionSig(fargs, _) = fsig in
        let _ = 
          List.map 
            (fun (FunctionArg(id, ts)) -> 
              add_id body_scope id (gotype_of_typspec ctx ts) ln )
            fargs
        in    
        let () = List.iter (tc_statement body_scope) body in
        let ret_stmts = 
          List.filter 
            (function 
             | LinedStatement(line, ReturnStatement _) -> true 
             | _ -> false)
            body 
        in
        let ret_exps = 
          List.map 
            (function 
             | LinedStatement(line, ReturnStatement e) -> (line, e) 
             | _ -> raise (InternalError "Non return statement. Filter properly.")  )
            ret_stmts
        in
        let () = List.iter
          (fun (ln, e) -> match e, ret_type with
           | None, None -> ()
           | Some(e), None -> raise (StmtTypeCheckError ("Too many arguments to return", ln))
           | None, Some(t) -> raise (StmtTypeCheckError ("Not enough arguments to return", ln))
           | Some(e1), Some(t) when (get_expression_type body_scope e1 = t) -> ()
           | Some(e1), Some(t) -> 
              raise 
                (StmtTypeCheckError
                  ( "The return type of the function is " ^ (string_of_type t) ^
                    " but this statement is returning an expression of type " ^
                    (string_of_type (get_expression_type body_scope e1)), ln ) ) ) 
          ret_exps
        in close_scope body_scope

  | TypeDeclBlock(std_list) -> 
      List.iter (tc_type_declaration ln ctx) std_list
  | VarDeclBlock(mvd_list) -> 
      List.iter (tc_multiple_var_declaration ln ctx) mvd_list

and tc_multiple_var_declaration ln ctx = function
  | MultipleVarDecl(svd_list) -> List.iter (tc_single_var_declaration ln ctx) svd_list

and tc_single_var_declaration ln ctx = function
  | SingleVarDecl(id, typ_spec, exp) -> 
    if (id_in_current_scope ctx id) then (* Cannot be a redeclaration in the same scope *)
      raise (VariableRedeclaration (prev_decl_msg ctx id))
    else
      let decl_type = (match typ_spec with  (* Determine the declared type *)
      | None -> None
      | Some(t) -> Some(gotype_of_typspec ctx t))
      in
      let exp_type = (match exp with  (* Determine the expression type *)
      | None -> None
      | Some(e) -> Some(get_expression_type ctx e))
      in
      match decl_type, exp_type with
      | None, None -> raise (TypeCheckError "Invalid variable declration") (* This is not even a valid declaration *)
      | Some(t), None -> add_id ctx id t ln  (* Sure *)
      | None, Some(t) -> add_id ctx id t ln  (* Okay *)
      | (Some(t1), Some(t2)) when (t1 = t2) -> add_id ctx id t1 ln (* Cool *)
      | Some(t1), Some(t2) ->  (* Mismatching type. I no longer know what to do *)
          raise (
            TypeCheckError ("Variable " ^ (string_of_id id) ^ " is declared to be of type " ^ 
              (string_of_type t1) ^ " but the initial value is of type " ^
              (string_of_type t2)
            )
          )

and tc_type_declaration ln ctx = function
  | SingleTypeDecl(id, ts) -> 
    if (id_in_current_scope ctx id) then 
      raise (VariableRedeclaration (prev_decl_msg ctx id))
    else
      let new_type = NewType (gotype_of_typspec ctx ts) in
      add_id ctx id new_type ln

and tc_expression ctx node = let _ = get_expression_type ctx node in ()

and tc_statement ctx = function
  | LinedStatement(ln, s) -> 
      ( try tc_plain_statement ln ctx s
        with TypeCheckError s ->
          fprintf err_channel "Typing Error at line %d:\n" ln;
          fprintf err_channel "%s\n" s;
          raise Abort )
and tc_plain_statement ln ctx = function
  | EmptyStatement -> ()
  | BreakStatement -> ()
  | ContinueStatement -> ()
  | ExpressionStatement e -> 
      (* Check for a void function first *)
      if is_void_function_call ctx e then ()
      else tc_expression ctx e
  | ReturnStatement(Some(e)) -> tc_expression ctx e
  | ReturnStatement(None) -> ()
  | VarDeclBlockStatement mvd_list -> List.iter (tc_multiple_var_declaration ln ctx) mvd_list
  | TypeDeclBlockStatement std_list -> List.iter (tc_type_declaration ln ctx) std_list
  | ShortVarDeclStatement svd_list -> 
      let exps = List.map (fun (ShortVarDecl(_, exp)) -> exp) svd_list in
      let () = List.iter (tc_expression ctx) exps in
      let id_list = List.map (fun (ShortVarDecl(id, _)) -> id) svd_list in
      let all_old = List.fold_left (fun x id -> x && id_in_current_scope ctx id) true id_list in
      let () = 
        ( if all_old then 
            raise (TypeCheckError "No new variables in short variable declaration statement")
          else () ) in
      let check_svd (ShortVarDecl(id, exp)) =
        let rhs_type = get_expression_type ctx exp in
        if (not (id_in_current_scope ctx id)) then
          add_id ctx id rhs_type ln
        else
          let cur_type = lookup_id ctx id in
          if (cur_type == rhs_type) then ()
          else raise (TypeCheckError (assign_error_msg cur_type rhs_type) )
      in List.iter check_svd svd_list
  | BlockStatement stmt_list -> 
      let block_scope = open_scope ctx in
      let () = List.iter (tc_statement block_scope) stmt_list in
      close_scope block_scope

  | AssignmentStatement a_list -> 
      let left_exp_list = List.map (fun (x,y) -> x) a_list in
      let right_exp_list = List.map (fun (x,y) -> y) a_list in
      let () = List.iter (tc_expression ctx) left_exp_list in
      let () = List.iter (tc_expression ctx) right_exp_list in
      let check_assignment (e1, e2) =
        let e1_type = get_expression_type ctx e1 in
        let e2_type = get_expression_type ctx e2 in
        if e1_type = e2_type then ()
        else raise (TypeCheckError (assign_error_msg e1_type e2_type))
      in
      List.iter check_assignment a_list
  
  | PrintStatement exp_list
  | PrintlnStatement exp_list -> List.iter (tc_expression ctx) exp_list
  | ForStatement (s1, cond, s2, stmt_list) -> 
      let init_scope = open_scope ctx in
      let () = match s1 with
      | None -> ()
      | Some(s) -> tc_statement init_scope s
      in
      let () = match (get_expression_type init_scope cond) with
      | GoBool -> ()
      | _ -> raise (TypeCheckError "for loop condition type must be bool")
      in
      let () = match s2 with
      | None -> ()
      | Some(s) -> tc_statement init_scope s
      in
      let body_scope = open_scope init_scope in
      let () = List.iter (tc_statement body_scope) stmt_list in
      let () = close_scope body_scope in
      close_scope init_scope
  | IfStatement (init, exp, then_list, else_list) -> 
      let init_scope = open_scope ctx in
      let () = match init with
      | None -> ()
      | Some(s) -> tc_statement init_scope s
      in
      let () = match (get_expression_type init_scope exp) with
      | GoBool -> ()
      | _ -> raise (TypeCheckError "if loop condition type must be bool")
      in
      let then_scope = open_scope init_scope in
      let () = List.iter (tc_statement then_scope) then_list in
      let () = close_scope then_scope in
      let () = match else_list with
      | None -> ()
      | Some(stmt_list) -> 
          let else_scope = open_scope init_scope in
          let () = List.iter (tc_statement else_scope) stmt_list in
          close_scope else_scope
      in close_scope init_scope
  | SwitchStatement (init, exp, case_list) -> 
      let init_scope = open_scope ctx in
      let () = match init with
      | None -> ()
      | Some(s) -> tc_statement init_scope s
      in
      let switch_cond_type = get_expression_type init_scope exp in
      let check_switch_case par_ctx = function
      | SwitchCase(exp_list, stmt_list) -> 
          let () = List.iter
            ( fun x -> 
                let case_type = get_expression_type par_ctx x in
                if case_type = switch_cond_type then ()
                else raise (TypeCheckError 
                  ( "Case expression type " ^ (string_of_type case_type) ^
                  " does not match switch condition type " ^ (string_of_type switch_cond_type) ) ) )
            exp_list
          in
          let case_scope = open_scope par_ctx in
          let () = List.iter (tc_statement case_scope) stmt_list in
          close_scope case_scope
      | DefaultCase(stmt_list) -> 
          let case_scope = open_scope par_ctx in
          let () = List.iter (tc_statement case_scope) stmt_list in
          close_scope case_scope
      in
      let () = List.iter (check_switch_case init_scope) case_list in
      close_scope init_scope

let build_symbol_table ast =
  let global_scope = initial_scope ()  (* 1024 is the initial hash table size *)
  in tc_program global_scope ast
