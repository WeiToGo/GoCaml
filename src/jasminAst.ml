type jasmin_class = 
  { 
    source: string;
    field_list: jfield list;
    method_list: jmethod list;
    class_name: string;
  }
and jfield = Field of string * string
and jmethod = Method of jmstatement list 
(* Jasmin method statement *)
and jmstatement = Label of string | JInst of jinstruction
and jliteral = JLit of string
and jlocalindex = JLI of int  (* index to local variables pool *)
and jinstruction =
  | Iconst_0 | Iconst_1 | Iconst_2 | Iconts_3
  | Ldc of jliteral
  | Dup
  | BiPush of jliteral
  (* Keep adding more and more instructions here*)