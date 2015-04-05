exception NotImplemented


module GlobalVarMap = Map.Make(Utils.Int)
module LocalVarMap = Map.Make(Utils.Int)


type bytecode_ast = 
  { 
    source: string;
    top_level_vars: global_id_entry list;
    methods: jmethod list;
  }
and jmethod = 
  { 
    signature: jmethod_sig;
    code: jmstatement list;
    local_mapping: local_id_entry LocalVarMap.t;
  }
and global_id_entry = 
  { 
    name: string;
    var_number: int;
    jtype: jtype;
    init_code: jmstatement list;
  }
and local_id_entry = int (* index in locals array *) * jtype (* type *)
and jmethod_sig = 
  { 
    method_name: string;
    arg_types: jtype list; 
    return_type: jtype; 
  }
and jmstatement = (* Jasmin method statement *)
  | JLabel of string 
  | JInst of jinstruction
  | PS of pseudo_instruction
and jinstruction =
  | Iconst_0 | Iconst_1 | Iconst_2 | Iconts_3
  | Ldc of string
  | Dup
  | BiPush of string 
  | GetStatic of string * jtype
  | PutStatic of string * jtype
  | InvokeVirtual of jmethod_sig
  | Return
  (* Keep adding more and more instructions here.
   * Then also change the string_of_jinst function below *)
and pseudo_instruction = 
  | LoadVar of int
  | StoreVar of int 
and jtype = JVoid 
          | JByte 
          | JChar 
          | JShort 
          | JInt 
          | JLong 
          | JFloat 
          | JDouble 
          | JRef of string 
          | JArray of jtype
          | JStruct of ((string * jtype) list)

(* Bunch of utility functions *)
let flstring class_name field_name = class_name ^ "/" ^ field_name
let quote_string s = "\"" ^ s ^ "\""

let rec string_of_jtype = function
| JVoid -> "V"
| JByte -> raise NotImplemented
| JChar -> raise NotImplemented
| JShort -> raise NotImplemented
| JInt -> "I"
| JLong -> raise NotImplemented
| JFloat -> "F"
| JDouble -> raise NotImplemented
| JRef(s) -> "L" ^ s ^ ";"
| JArray(atype) -> "[" ^ (string_of_jtype atype)
| JStruct(s) -> raise NotImplemented

let rec string_of_jsig {method_name; arg_types; return_type;} = 
  method_name ^ "(" ^
  List.fold_left
    (fun x y -> x ^ (string_of_jtype y) )
    ""
    arg_types
  ^ ")" ^ (string_of_jtype return_type)

let string_of_jinst = function
| Iconst_0 -> "iconst_0"
| Iconst_1 -> "iconst_1"
| Iconst_2 -> "iconst_2"
| Iconts_3 -> "iconst_3"
| Ldc(s) -> "ldc " ^ s
| Dup -> "dup"
| BiPush(s) -> "bipush " ^ s 
| GetStatic(s, t) -> "getstatic " ^ s ^ " " ^ (string_of_jtype t)
| PutStatic(s, t) ->  "putstatic " ^ s ^ " " ^ (string_of_jtype t)
| InvokeVirtual(jsig) -> "invokevirtual " ^ (string_of_jsig jsig)
| Return -> "return"

let calculate_local_limit jstmts = 25  (* Not implemented yet *)
let calculate_ostack_limit jstmts = 25 (* Not implemented yet *) 

(*  A List of string constants so that you don't have to keep typing raw strings *)
(* jc = jvm constant *)
let jc_string = "java/lang/String"
let jc_printstream = "java/io/PrintStream"
let jc_sysout = "java/lang/System/out"
let jc_println = flstring jc_printstream "println"

let get_global_var_map gvar_entry_list = 
  List.fold_left
    (fun cur_map new_entry -> 
      let { var_number = n; _ } = new_entry in
      GlobalVarMap.add n new_entry cur_map )
    GlobalVarMap.empty
    gvar_entry_list

let real_instructions (global_map, local_map) pinst = raise NotImplemented