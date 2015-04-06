open JasminAst
open Ast
open Symtable

(* exceptions *)
exception ImproperType
exception NotImplemented
exception InternalError of string

(* helper functions *)

let string_of_id id = match id with
| ID(name, _) -> name 
| BlankID -> raise (InternalError("You attempted to convert a blank id to string. This can only bring you misery.")) 

let exp_type (Expression(_, type_ref)) = match !type_ref with
| None -> raise (InternalError("Empty expression type in AST. Did you typecheck the AST?"))
| Some t -> t
(*~~ read deal ~~*)

let get_gotype_from_typespec t = raise NotImplemented

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

let get_mapping_from_stmt stmt = LocalVarMap.empty

let get_mapping_from_go_arg (FunctionArg (id, _)) = 
  let sym_table_entry = match id with
  | ID(_, entry_ref) -> !entry_ref
  | BlankID -> raise (InternalError("Go home blank id.")) in
  match sym_table_entry with
  | None -> raise (InternalError("Empty id type in AST. Did you typecheck the AST?"))
  | Some(entry) -> 
    let Symtable.Entry(_, gotype, _, _, var_num) = entry in
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
| _ -> raise NotImplemented

let rec process_expression (Expression(e, t)) = match e with 
  | LiteralExp(lit) -> process_literal lit
  | _ -> raise NotImplemented

let process_var_decl mvd_list = raise NotImplemented

let rec process_statement (LinedStatement(_, s)) = match s with
| PrintlnStatement(exp_list) -> 
    (* get instructions for each expression *)
    let print_instructions = 
      List.map 
      (fun e -> 
        [ JInst(GetStatic(jc_sysout, JRef(jc_printstream))); ] @
        process_expression e @
        [ JInst(InvokeVirtual(
            { method_name = jc_println;
              arg_types = [get_jvm_type (exp_type e)]; 
              return_type = JVoid; } )); ] )
      exp_list in 
    List.flatten print_instructions
| _ -> raise NotImplemented

let process_func_decl id funsig stmt_list = 
  let method_name = string_of_id id in 
  let FunctionSig(go_function_args, go_retrun_type) = funsig in 
  let signature = if method_name = "main" then 
    {
      method_name = "main";
      arg_types = [JArray(JRef(jc_string))];
      return_type = JVoid;
    } else
    { 
      method_name =  string_of_id id;
      arg_types =
         List.map 
          (fun (FunctionArg(_, t)) ->
            get_jvm_type (get_gotype_from_typespec t))
          go_function_args ;
      return_type = 
        match go_retrun_type with
        | None -> JVoid
        | Some(t) -> get_jvm_type (get_gotype_from_typespec t); 
    } in 
  let merge_maps k xo yo = match xo,yo with
  | Some x, Some y -> raise (InternalError("Same variable present in two maps. This should be impossible."))
  | Some x, None -> xo
  | None, Some y -> yo
  | None, None -> None in
  let map_with_args = 
        List.fold_left
          (fun old_map (var_num, local_entry) -> 
            LocalVarMap.add var_num (var_num, local_entry) old_map)
          LocalVarMap.empty
          (List.map get_mapping_from_go_arg go_function_args)
  in
  let maps_from_stmts = List.map get_mapping_from_stmt stmt_list in
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
| VarDeclBlock(mvd_list) -> process_var_decl mvd_list

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
