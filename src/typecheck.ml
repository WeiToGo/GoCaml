open TypedAst
exception NotImplemented

module StructFields = Map.Make(String)

type gotype = 
      | GoInt 
      | GoFloat 
      | GoBool 
      | GoRune 
      | GoString
      | GoSlice of gotype
      | GoArray of (int * gotype)
      | GoStruct of (gotype StructFields.t)
      | GoFunction of (gotype list * gotype)  (* GoFunction of (argument list, return type) *)
      | GoCustom of (string * gotype)  (* CustomType of name of custom type * the return type *)
      | NewType of gotype  (* This is the type for newly defined types. 
                              e.g. `type length int` will put `("length", NewType(GoInt))` in the 
                              symbol table *)

type scope = Scope of (scope option * (string, gotype) Hashtbl.t)

let ctx_add scope symbol typ = 
  let Scope(_, hash) = scope in
  Hashtbl.add hash symbol typ

let create_typed_ast ast =
  let rec tc_program node ctx = raise NotImplemented
  and tc_package_decl node ctx = raise NotImplemented
  and tc_top_decl node ctx = raise NotImplemented
  and tc_multiple_var_declaration node ctx = raise NotImplemented
  and tc_single_var_declaration node ctx = raise NotImplemented
  and tc_short_var_decl node ctx = raise NotImplemented
  and tc_type_declaration node ctx = raise NotImplemented
  and tc_type_spec node ctx = raise NotImplemented
  and tc_multi_struct_field_decl node ctx = raise NotImplemented
  and tc_single_struct_field_decl node ctx = raise NotImplemented
  and tc_basic_type node ctx = raise NotImplemented
  and tc_identifier node ctx = raise NotImplemented
  and tc_function_signature node ctx = raise NotImplemented
  and tc_function_arg node ctx = raise NotImplemented
  and tc_expression node ctx = raise NotImplemented
  and tc_literal node ctx = raise NotImplemented
  and tc_int_literal node ctx = raise NotImplemented
  and tc_unary_op node ctx = raise NotImplemented
  and tc_binary_op node ctx = raise NotImplemented
  and tc_statement node ctx = raise NotImplemented
  and tc_plain_statement node ctx = raise NotImplemented
  and tc_switch_case node ctx = raise NotImplemented
  in
  let global_scope = Scope(None, Hashtbl.create 1024)  (* 1024 is the initial hash table size *)
  in tc_program ast global_scope