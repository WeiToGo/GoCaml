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
| GoCustom(name, t) -> t
| NewType(t) -> NewType (resolve_to_base t)

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
| _ -> raise NotImplemented

(* | BinaryExp of (binary_op * expression * expression)
| FunctionCallExp of (expression * (expression list))
| AppendExp of (identifier * expression)
| TypeCastExp of (type_spec * expression)
| IndexExp of (expression * expression)
| SelectExp of (expression * identifier) *)



let gotype_of_function_sig ctx fsig = 
  let FunctionSig(args_list, ret) = fsig in
  let arg_types = List.map (fun (FunctionArg (id, ts)) -> gotype_of_typspec ctx ts) args_list in
  let ret_type = (match ret with
      | None -> None
      | Some(t) -> Some (gotype_of_typspec ctx t) )
  in
  GoFunction (arg_types, ret_type)




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
           | Some(e1), Some(t) when get_expression_type body_scope e1 == t -> ()
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
and tc_short_var_decl ctx node = raise NotImplemented

and tc_type_declaration ln ctx = function
  | SingleTypeDecl(id, ts) -> 
    if (id_in_current_scope ctx id) then 
      raise (VariableRedeclaration (prev_decl_msg ctx id))
    else
      let new_type = NewType (gotype_of_typspec ctx ts) in
      add_id ctx id new_type ln

and tc_expression ctx node = let _ = get_expression_type ctx node in ()

and tc_statement ctx = function
  | LinedStatement(ln, s) -> tc_plain_statement ln ctx s
and tc_plain_statement ln ctx = function
  | EmptyStatement -> ()
  | BreakStatement -> ()
  | ContinueStatement -> ()
  | ExpressionStatement e -> tc_expression ctx e
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
  | _ -> raise NotImplemented
and tc_switch_case ctx node = raise NotImplemented

let build_symbol_table ast =
  let global_scope = initial_scope ()  (* 1024 is the initial hash table size *)
  in tc_program global_scope ast
