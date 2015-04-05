(* use this file in utop. #use "path/to/code_emitter_test.ml" *)
(* This is outdated *)

open CodeEmitter;;
open JasminAst;;

(* The jasmin file that should be emitted is in TargetCodeEmit.j *)

let main_class = 
{ source = "code_emitter_test.java";
  class_name = "CodeEmitTest";

  field_list = 
    [
      JField("str_field", JRef(jc_string));
      JField("int_field", JInt);
    ];

  method_list = 
    [
      JMethod(
        JSig("main", [JArray(JRef(jc_string));], JVoid),
        [
          JInst(GetStatic(jc_sysout, JRef(jc_printstream)));
          JInst(BiPush("42"));
          JLabel("SillyLabel");
          JInst(InvokeVirtual(
                  JSig(jc_println, [JInt;], JVoid); ) );
          JInst(Return);
        ] );
    ];

  clinit = 
    [
      JInst(Ldc(quote_string "hello world!"));
      JInst(PutStatic(flstring "CodeEmitTest"  "str_field", JRef(jc_string)));
      JInst(BiPush("0"));
      JInst(PutStatic(flstring "CodeEmitTest"  "int_field", JInt));
      JInst(Return);
    ];
} in

print_class main_class;;