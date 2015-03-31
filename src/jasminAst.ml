exception NotImplemented

type jasmin_class = 
  { 
    source: string;
    field_list: jfield list;
    method_list: jmethod list;
    class_name: string;
    clinit: jmstatement list;
  }
and jfield = JField of string * jtype
and jmethod = JMethod of jmethod_sig  * jmstatement list 
and jmethod_sig = JSig of string * (jtype list) * jtype  (* name of method * argument type list * return type *)
(* Jasmin method statement *)
and jmstatement = JLabel of string | JInst of jinstruction
and jlocalindex = JLI of int  (* index to local variables pool *)
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

let rec string_of_jsig (JSig(method_name, arg_types, ret_type)) = 
  method_name ^ "(" ^
  List.fold_left
    (fun x y -> x ^ (string_of_jtype y) )
    ""
    arg_types
  ^ ")" ^ (string_of_jtype ret_type)

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
