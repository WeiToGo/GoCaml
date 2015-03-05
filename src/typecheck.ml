open TypedAst
open Printf
exception NotImplemented

(* globals *)

module StructFields = Map.Make(String)

let out_channel = stdout

let dumpsymtab = true

let dumpsymtab_all = true


(* types *)

type gotype = 
      | GoInt 
      | GoFloat 
      | GoBool 
      | GoRune 
      | GoString
      | GoSlice of gotype
      | GoArray of (int * gotype)
      | GoStruct of (gotype StructFields.t)
      | GoFunction of (gotype list * gotype option)  (* GoFunction of (argument list, return type) *)
      | GoCustom of (string * gotype)  (* CustomType of name of custom type * the return type *)
      | NewType of gotype  (* This is the type for newly defined types. 
                              e.g. `type length int` will put `("length", NewType(GoInt))` in the 
                              symbol table *)

type scope = Scope of (scope option * (string, sym_table_entry) Hashtbl.t)
             (* Scope of parent * symbol hashtable *)
and sym_table_entry = Entry of (string * gotype * scope * int)
                       (* Entry of true_name, its gotype,
                           the scope it was declared in, and the declaration line *)


(* --- Printing functions --- *)

let rec string_of_type typ = match typ with
  | GoInt -> "int"
  | GoFloat -> "float64"
  | GoBool -> "bool"
  | GoRune -> "rune"
  | GoString -> "string"
  | GoSlice(slice_type) -> "[]" ^ (string_of_type slice_type)
  | GoArray(size, arr_type) -> 
      "[" ^ 
      (string_of_int size) ^
      "]" ^
      (string_of_type arr_type) 
  | GoStruct(fields) -> "struct " ^ (string_of_struct_fields fields)
  | GoFunction(args, ret) -> 
      "func (" ^ 
      (string_of_args_list args) ^ ")" ^
      ( 
        match ret with
        | None -> ""
        | Some(typ) -> " " ^ string_of_type typ
      )
  | GoCustom(name, typ) -> name (* ^ " (" ^ (string_of_type typ) ^ ")" *)
  | NewType(typ) -> "type: " ^ (string_of_type typ)

and string_of_struct_fields fields = 
  let fields_list = StructFields.bindings fields in
  let rec string_of_sf_list = function
  | a :: (b :: _ as t) -> 
      (string_of_field a) ^ ", " ^ (string_of_sf_list t)
  | h :: [] -> string_of_field h
  | [] -> ""
  in
  "{ " ^ (string_of_sf_list fields_list) ^ " }"

and string_of_field struct_field = 
  let (field_name, field_type) = struct_field in
  field_name ^ ": " ^ (string_of_type field_type)

and string_of_args_list args = match args with
  | a :: (b :: _ as t) -> 
      (string_of_type a) ^
      ", " ^ 
      (string_of_args_list t)
  | h :: [] -> string_of_type h
  | [] -> ""



let print_entry key entry = 
  let Entry(id_name, typ, _, _) = entry in
  fprintf out_channel "%s" id_name;
  fprintf out_channel " -> ";
  fprintf out_channel "%s" (string_of_type typ);
  fprintf out_channel "\n";
  ()


let rec print_sym_table scope = 
  let Scope(_, hashmap) = scope in 
  Hashtbl.iter print_entry hashmap


(* --- Symbol Table utils --- *)

(* Adds a symbol with type typ to scope *)
let add_sym scope symbol typ ln = 
  let Scope(_, hashmap) = scope in
  let entry = Entry(symbol, typ, scope, ln) in
  let () = Hashtbl.replace hashmap symbol entry in
  scope

(* Opens a new scope with given parent. *)
let open_scope parent_scope = 
  let new_scope = Scope(Some parent_scope, Hashtbl.create 1024) in
  new_scope

(* Closes scope. It only involves printing out the symbol table if proper flags are set.
   Returns unit *)
let close_scope scope = 
  if dumpsymtab then print_sym_table scope
  else ()

(* Returns the initial global scope with true and false pre-declared *)
let initial_scope () = 
  let scp = Scope(None, Hashtbl.create 1024) in
  let scp = add_sym scp "true" GoBool 0 in
  let scp = add_sym scp "false" GoBool 0 in
  scp

(* ------------- *)

let build_symbol_table ast =
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
  let global_scope = initial_scope ()  (* 1024 is the initial hash table size *)
  in tc_program ast global_scope