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

let prev_decl_msg scope sym = 
  let entry = lookup_current scope sym in
  let Entry(_, _, _, ln) = entry in
  "Previous declaration of " ^ sym ^ " at line " ^
  (string_of_int ln) ^ "."


(* --~~~~~~--*** Helper Functions ***--~~~~~~-- *)

let type_of_entry entry =
  let Entry(_, typ, _, _) = entry in 
  typ 

(* Adds the symbol for an identifier to scope, and also updates the ref field of id to point to the symbol table entry. *)
let add_id scope id typ ln = match id with
  | BlankID -> ()
  | ID(name, sym_ref) -> 
      let entry = add_sym scope name typ ln in
      sym_ref := Some(entry)

(* Looks up id in scope. If found, updates the ref field of id to point to the symbol table entry. *)
(* Early exit if id already contains reference to symbol table entry *)
let lookup_id scope id = match id with
| BlankID -> raise (InternalError "Attempted lookup of BlankID in symbol table")
| ID(name, sym_ref) -> match !sym_ref with
  | None -> 
      let entry = lookup scope name in
      sym_ref := Some(entry); type_of_entry entry
  | Some(entry) -> type_of_entry entry 

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
  | StructType(msfd_list) -> GoStruct(map_of_struct_fields msfd_list)
  | FunctionType(ts_list, ts_ret) -> (
      let args = List.map (gotype_of_typspec ctx) ts_list in
      match ts_ret with
      | None -> GoFunction(args, None)
      | Some(t) -> GoFunction(args, Some(gotype_of_typspec ctx t))
  )
  | CustomType(id) -> ( match id with
    | BlankID -> raise (TypeCheckError "Blank ID cannot be used as a type")
    | ID(name, _) -> (
        try lookup_id ctx id
        with Not_found -> raise (TypeCheckError (name ^ " was not previously declared as a type"))
    )
  )

and map_of_struct_fields msfd_list = raise NotImplemented

let get_expression_type ctx e = raise NotImplemented

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
      with (TypeCheckError s) -> 
        fprintf err_channel "%s" ("Typing Error at line " ^ (string_of_int ln) ^ ":\n");
        fprintf err_channel "%s" s

and tc_top_decl ln ctx = function
  | FunctionDecl(_, _, _) -> raise NotImplemented
  | TypeDeclBlock(_) -> raise NotImplemented
  | VarDeclBlock(mvd_list) -> 
      List.iter (tc_multiple_var_declaration ln ctx) mvd_list

and tc_multiple_var_declaration ln ctx = function
  | MultipleVarDecl(svd_list) -> List.iter (tc_single_var_declaration ln ctx) svd_list

and tc_single_var_declaration ln ctx = function
  | SingleVarDecl(id, typ_spec, exp) -> 
    match id with
    | BlankID -> () (* if id is blank_id do nothing *)
    | ID(name, sym_entry) ->  (* Otherwise:  *)
        if (in_current_scope ctx name) then (* Cannot be a redeclaration in the same scope *)
          raise (VariableRedeclaration (prev_decl_msg ctx name))
        else
          let decl_type = (match typ_spec with  (* Determine the declared type *)
          | None -> None
          | Some(t) -> Some(gotype_of_typspec ctx t)) in
          let exp_type = (match exp with  (* Determine the expression type *)
          | None -> None
          | Some(e) -> Some(get_expression_type ctx e)) in
          match decl_type, exp_type with
          | None, None -> raise (TypeCheckError "Invalid variable declration") (* This is not even a valid declaration *)
          | Some(t), None -> add_id ctx id t ln  (* Sure *)
          | None, Some(t) -> add_id ctx id t ln  (* Okay *)
          | (Some(t1), Some(t2)) when (t1 = t2) -> add_id ctx id t1 ln (* Cool *)
          | Some(t1), Some(t2) ->  (* Mismatching type. I no longer know what to do *)
              raise (
                TypeCheckError ("Variable " ^ name ^ " is declared to be of type " ^ 
                  (string_of_type t1) ^ " but the initial value is of type " ^
                  (string_of_type t2)
                )
              )
and tc_short_var_decl ctx node = raise NotImplemented
and tc_type_declaration ctx node = raise NotImplemented
and tc_type_spec ctx node = raise NotImplemented
and tc_multi_struct_field_decl ctx node = raise NotImplemented
and tc_single_struct_field_decl ctx node = raise NotImplemented
and tc_basic_type ctx node = raise NotImplemented
and tc_identifier ctx node = raise NotImplemented
and tc_function_signature ctx node = raise NotImplemented
and tc_function_arg ctx node = raise NotImplemented
and tc_expression ctx node = raise NotImplemented
and tc_literal ctx node = raise NotImplemented
and tc_int_literal ctx node = raise NotImplemented
and tc_unary_op ctx node = raise NotImplemented
and tc_binary_op ctx node = raise NotImplemented
and tc_statement ctx node = raise NotImplemented
and tc_plain_statement ctx node = raise NotImplemented
and tc_switch_case ctx node = raise NotImplemented

let build_symbol_table ast =
  let global_scope = initial_scope ()  (* 1024 is the initial hash table size *)
  in tc_program global_scope ast
