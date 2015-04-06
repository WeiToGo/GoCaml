open Printf


(* globals *)

let out_channel  = ref stdout
let err_channel = ref stderr

let dumpsymtab = ref false

let dumpsymtab_all = ref false

module StructFields = Map.Make(String)
let next_count = Utils.new_counter 0


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
and sym_table_entry = Entry of (string * gotype * scope * int * int)
                       (* Entry of true_name, its gotype,
                           the scope it was declared in, the declaration line, and a global serial *)


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
  let Entry(id_name, typ, _, ln, serial) = entry in
  fprintf (! out_channel)  "%s" id_name;
  fprintf (! out_channel)  " -> ";
  fprintf (! out_channel)  "%s (line %s) $%d" (string_of_type typ) (string_of_int ln) serial;
  fprintf (! out_channel)  "\n";
  ()

(* Prints all the entries in scope. Does not print parent scopes *)
let rec print_sym_table scope = 
  let Scope(_, hashmap) = scope in 
  Hashtbl.iter print_entry hashmap


(* --- Symbol Table utils --- *)

(* Adds a symbol with type typ to scope. Returns the sym table entry *)
let add_sym scope symbol typ ln = 
  let Scope(_, hashmap) = scope in
  let entry = Entry(symbol, typ, scope, ln, next_count ()) in
  let () = Hashtbl.replace hashmap symbol entry in
  entry

(* What level are you in? (inefficient implementation) *)
let scope_level scope = 
  let rec aux scope depth = match scope with
  | Scope(Some(par), _) -> aux par (depth + 1)
  | Scope(None, _) -> depth
  in
  aux scope 0 


(* Opens a new scope with given parent. *)
let open_scope parent_scope = 
  let new_scope = Scope(Some parent_scope, Hashtbl.create 1024) in
  new_scope

(* Closes scope. It only involves printing out the symbol table if proper flags are set.
   Returns unit *)
let close_scope scope = 
  if ! dumpsymtab then
    ( fprintf (! out_channel)  "---- Exiting scope (level %d) --- \n" (scope_level scope);
      print_sym_table scope;
      fprintf (! out_channel)  "----------------------\n";
      )
  else ()


(* Returns the initial global scope with true and false pre-declared *)
let initial_scope () = 
  let scp = Scope(None, Hashtbl.create 1024) in
  let _ = add_sym scp "true" GoBool 0 in
  let _ = add_sym scp "false" GoBool 0 in
  scp

(* Is sym in the current scope? *)
let in_current_scope scope sym = 
  let Scope(_, hashmap) = scope in
  Hashtbl.mem hashmap sym

let lookup_current scope sym = 
  let Scope(_, hashmap) = scope in
  Hashtbl.find hashmap sym

(* Recursive lookup for sym. Returns the whole entry *)
let rec lookup scope sym = 
  let Scope(par, hashmap) = scope in
  try
    Hashtbl.find hashmap sym
  with Not_found -> 
    match par with
    | None -> raise Not_found
    | Some(par_scp) -> lookup par_scp sym

(* Recursive lookup for sym. Returns only the type *)
let lookup_type scope sym = 
  let Entry(_, typ, _, _, _) = lookup scope sym in 
  typ
