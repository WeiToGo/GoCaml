exception NotImplemented
exception InternalError of string

module GlobalVarMap = Map.Make(Utils.Int)
module LocalVarMap = Map.Make(Utils.Int)

let main_class_name = "GeneratedBytecode"

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
  | Iconst_0 | Iconst_1 | Iconst_2 | Iconst_3 | Iconst_m1
  | Ldc of string
  | Dup
  | BiPush of string  (* -128 to 127*)
  | GetStatic of string * jtype
  | PutStatic of string * jtype
  | InvokeVirtual of jmethod_sig
  | InvokeStatic of jmethod_sig
  | Return
  | Pop
  | Iload of int
  | Dload of int
  | Aload of int
  | IStore of int
  | DStore of int
  | AStore of int
  | ICmpeq of string
  | ICmpne of string
  | ICmplt of string
  | ICmpgt of string
  | ICmple of string
  | ICmpge of string
  | DCmpg
  | Ifeq of string
  | Ifne of string
  | Iadd | Isub | Imul | Idiv | Irem | Ior | Ixor
  | Ishl | Ishr | Iand | Ineg 
  | Dadd | Dsub | Dmul | Ddiv | Drem | Dneg
  | Fadd (* test for float*)
  | AConstNull
  | Goto of string

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
| JChar -> "C"
| JShort -> raise NotImplemented
| JInt -> "I"
| JLong -> "J"
| JFloat -> "F"
| JDouble -> "D"
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
| Iconst_3 -> "iconst_3"
| Iconst_m1 -> "iconst_m1"
| Ldc(s) -> "ldc " ^ s
| Dup -> "dup"
| BiPush(s) -> "bipush " ^ s 
| GetStatic(s, t) -> "getstatic " ^ s ^ " " ^ (string_of_jtype t)
| PutStatic(s, t) -> "putstatic " ^ s ^ " " ^ (string_of_jtype t)
| InvokeVirtual(jsig) -> "invokevirtual " ^ (string_of_jsig jsig)
| InvokeStatic(jsig) -> "invokestatic " ^ (string_of_jsig jsig)
| Return -> "return"
| Iload(i) -> "iload " ^ (string_of_int i)
| Dload(i) -> "dload " ^ (string_of_int i)
| Aload(i) -> "aload " ^ (string_of_int i) 
| IStore(i) -> "istore " ^ (string_of_int i)
| DStore(i) -> "dstore " ^ (string_of_int i) 
| AStore(i) -> "astore " ^ (string_of_int i) 
| ICmpeq(l) -> "if_icmpeq " ^ l
| ICmpne(l) -> "if_icmpne " ^ l
| ICmplt(l) -> "if_icmplt " ^ l
| ICmpgt(l) -> "if_icmpgt " ^ l
| ICmple(l) -> "if_icmple " ^ l
| ICmpge(l) -> "if_icmpge " ^ l
| DCmpg -> "dcmpg"
| Ifeq(l) -> "ifeq" ^ l  
| Ifne(l) -> "ifne" ^ l
| Iadd -> "iadd"
| Isub -> "isub"
| Imul -> "imul"
| Idiv -> "idiv"
| Irem -> "irem"
| Ior -> "ior"
| Ixor -> "ixor"
| Ishl -> "ishl"
| Ishr -> "ishr"
| Iand -> "iand"
| Ineg -> "ineg"
| Dadd -> "dadd"
| Dsub -> "dsub"
| Dmul -> "dmul"
| Ddiv -> "ddiv"
| Drem -> "drem"
| Dneg -> "dneg"
| Fadd -> "fadd" (* test for float*)
| Pop -> "pop" 
| AConstNull -> "aconst_null"
| Goto(l) -> "goto " ^ l


let calculate_local_limit jstmts = 25  (* Not implemented yet *)
let calculate_ostack_limit jstmts = 25 (* Not implemented yet *) 

(*  A List of string constants so that you don't have to keep typing raw strings *)
(* jc = jvm constant *)
let jc_string = "java/lang/String"
let jc_printstream = "java/io/PrintStream"
let jc_sysout = "java/lang/System/out"
let jc_println = flstring jc_printstream "println"
let jc_print = flstring jc_printstream "print"
let jcr_booltostring = "RuntimeSupport/booltostring"

(* Runtime method sigs *)
let jcr_booltostring = {
  method_name = "RuntimeSupport/booltostring";
  arg_types = [JInt];
  return_type = JRef(jc_string);
}

let get_global_var_map gvar_entry_list = 
  List.fold_left
    (fun cur_map new_entry -> 
      let { var_number = n; _ } = new_entry in
      
      GlobalVarMap.add n new_entry cur_map )
    GlobalVarMap.empty
    gvar_entry_list

let local_load_instructions lindex jvm_type = match jvm_type with
| JInt -> [JInst(Iload(lindex))]
| JFloat -> [JInst(Dload(lindex))]
| JRef _ -> [JInst(Aload(lindex))]
| _ -> raise NotImplemented

let global_load_instructions name jvm_type = 
  [JInst(GetStatic(main_class_name ^ "/" ^ name, jvm_type))] 

let local_store_instructions lindex jvm_type = match jvm_type with
| JInt -> [JInst(IStore(lindex))]
| JFloat -> [JInst(DStore(lindex))]
| JRef _ -> [JInst(AStore(lindex))]
| _ -> raise NotImplemented

let global_store_instructions name jvm_type = 
  [JInst(PutStatic(main_class_name ^ "/" ^ name, jvm_type))]

let real_statements (global_map, local_map) pinst = 
  let lookup_in_local var_num = 
    try let lindex, jtype = LocalVarMap.find var_num local_map in Some(lindex, jtype)
    with Not_found -> None
  in
  let lookup_in_global var_num =
    try let {name; jtype; _} = GlobalVarMap.find var_num global_map in Some(name,jtype)
    with Not_found -> None
  in 
  match pinst with
  | LoadVar(var_num) -> ( match lookup_in_global var_num, lookup_in_local var_num with
    | None, Some(i, jt) -> local_load_instructions i jt
    | Some(name, jt), None -> global_load_instructions name jt
    | Some(_, ja), Some(_, jb) -> raise (InternalError("Same variable defined with both local and global map."))
    | None, None -> match var_num with
      | 0 -> [ JInst(Iconst_1) ]  (* literal true *)
      | 1 -> [ JInst(Iconst_0) ]  (* literal false *)
      | _ -> raise (InternalError("Varibale could not be located in any map")) )
  | StoreVar(var_num) -> ( match lookup_in_global var_num, lookup_in_local var_num with
    | None, Some(i, jt) -> local_store_instructions i jt
    | Some(name, jt), None -> global_store_instructions name jt
    | _ -> raise (InternalError("Same variable defined with both local and global map.")) )
