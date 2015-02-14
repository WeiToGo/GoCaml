(* Helper printing functions for better testing *)
(* NOTE: This is NOT the pretty-printer *)

open Parser

let print_token tk = match tk with
| TPLUS -> print_endline "TPLUS";
| TMINUS -> print_endline "TMINUS";
| TMULT -> print_endline "TMULT";
| TDIV -> print_endline "TDIV";
| TMOD -> print_endline "TMOD";
| TBITAND -> print_endline "TBITAND";
| TBITOR -> print_endline "TBITOR";
| TBITXOR -> print_endline "TBITXOR";
| TLSFT -> print_endline "TLSFT";
| TRSFT -> print_endline "TRSFT";
| TANOT -> print_endline "TANOT";
| TADDAS -> print_endline "TADDAS";
| TSUBAS -> print_endline "TSUBAS";
| TMULAS -> print_endline "TMULAS";
| TDIVAS -> print_endline "TDIVAS";
| TMODAS -> print_endline "TMODAS";
| TANDAS -> print_endline "TANDAS";
| TORAS -> print_endline "TORAS";
| TXORAS -> print_endline "TXORAS";
| TLAS -> print_endline "TLAS";
| TRAS -> print_endline "TRAS";
| TANEQ -> print_endline "TANEQ";
| TAND -> print_endline "TAND";
| TOR -> print_endline "TOR";
| TREC -> print_endline "TREC";
| TINC -> print_endline "TINC";
| TEQ -> print_endline "TEQ";
| TLS -> print_endline "TLS";
| TGR -> print_endline "TGR";
| TASSIGN -> print_endline "TASSIGN";
| TNOT -> print_endline "TNOT";
| TNEQ -> print_endline "TNEQ";
| TLSEQ -> print_endline "TLSEQ";
| TGREQ -> print_endline "TGREQ";
| TCOLEQ -> print_endline "TCOLEQ";
| TTD -> print_endline "TTD";
| TLPAR -> print_endline "TLPAR";
| TRPAR -> print_endline "TRPAR";
| TLBR -> print_endline "TLBR";
| TRBR -> print_endline "TRBR";
| TLCUR -> print_endline "TLCUR";
| TRCUR -> print_endline "TRCUR";
| TCOM -> print_endline "TCOM";
| TDOT -> print_endline "TDOT";
| TSEMCOL -> print_endline "TSEMCOL";
| TCOL -> print_endline "TCOL";
| TSTR(st) -> print_endline "TSTR: "; print_endline st;
| TRWSTR(st) -> print_endline "TRWSTR"; print_endline st;
| TRUNE(st) -> print_endline "TRUNE"; print_endline st;
| ID(st) -> print_endline ("ID<" ^ st ^ ">");
| TEOF -> print_endline "TEOF";
| BREAK -> print_endline "BREAK";
| CASE -> print_endline "CASE";
| CHAN -> print_endline "CHAN";
| CONST -> print_endline "CONST";
| CONT -> print_endline "CONT";
| DEFAULT -> print_endline "DEFAULT";
| DEFER -> print_endline "DEFER";
| ELSE -> print_endline "ELSE";
| FALLTHROUGH -> print_endline "FALLTHROUGH";
| FOR -> print_endline "FOR";
| FUNC -> print_endline "FUNC";
| GO -> print_endline "GO";
| GOTO -> print_endline "GOTO";
| IF -> print_endline "IF";
| IMPORT -> print_endline "IMPORT";
| INTERFACE -> print_endline "INTERFACE";
| MAP -> print_endline "MAP";
| PACKAGE -> print_endline "PACKAGE";
| RANGE -> print_endline "RANGE";
| RETURN -> print_endline "RETURN";
| SELECT -> print_endline "SELECT";
| STRUCT -> print_endline "STRUCT";
| SWITCH -> print_endline "SWITCH";
| TYPE -> print_endline "TYPE";
| VAR -> print_endline "VAR";
| INT_TYP -> print_endline "INT_TYP";
| FL_TYP -> print_endline "FL_TYP";
| BOOL_TYP -> print_endline "BOOL_TYP";
| RUNE_TYP -> print_endline "RUNE_TYP";
| STR_TYP -> print_endline "STR_TYP";
| PRINT -> print_endline "PRINT";
| PRINTLN -> print_endline "PRINTLN";
| APPEND  -> print_endline "APPEND";
()

let string_of_token tk = match tk with
| TPLUS ->  "TPLUS"
| TMINUS ->  "TMINUS"
| TMULT ->  "TMULT"
| TDIV ->  "TDIV"
| TMOD ->  "TMOD"
| TBITAND ->  "TBITAND"
| TBITOR ->  "TBITOR"
| TBITXOR ->  "TBITXOR"
| TLSFT ->  "TLSFT"
| TRSFT ->  "TRSFT"
| TANOT ->  "TANOT"
| TADDAS ->  "TADDAS"
| TSUBAS ->  "TSUBAS"
| TMULAS ->  "TMULAS"
| TDIVAS ->  "TDIVAS"
| TMODAS ->  "TMODAS"
| TANDAS ->  "TANDAS"
| TORAS ->  "TORAS"
| TXORAS ->  "TXORAS"
| TLAS ->  "TLAS"
| TRAS ->  "TRAS"
| TANEQ ->  "TANEQ"
| TAND ->  "TAND"
| TOR ->  "TOR"
| TREC ->  "TREC"
| TINC ->  "TINC"
| TEQ ->  "TEQ"
| TLS ->  "TLS"
| TGR ->  "TGR"
| TASSIGN ->  "TASSIGN"
| TNOT ->  "TNOT"
| TNEQ ->  "TNEQ"
| TLSEQ ->  "TLSEQ"
| TGREQ ->  "TGREQ"
| TCOLEQ ->  "TCOLEQ"
| TTD ->  "TTD"
| TLPAR ->  "TLPAR"
| TRPAR ->  "TRPAR"
| TLBR ->  "TLBR"
| TRBR ->  "TRBR"
| TLCUR ->  "TLCUR"
| TRCUR ->  "TRCUR"
| TCOM ->  "TCOM"
| TDOT ->  "TDOT"
| TSEMCOL ->  "TSEMCOL"
| TCOL ->  "TCOL"
| TSTR(st) ->  "TSTR<" ^ st ^ ">"
| TRWSTR(st) ->  "TRWSTR<" ^ st ^ ">"
| TRUNE(st) ->  "TRUNE<" ^ st ^ ">"
| ID(st) -> "ID<" ^ st ^ ">"
| TEOF ->  "TEOF"
| BREAK ->  "BREAK"
| CASE ->  "CASE"
| CHAN ->  "CHAN"
| CONST ->  "CONST"
| CONT ->  "CONT"
| DEFAULT ->  "DEFAULT"
| DEFER ->  "DEFER"
| ELSE ->  "ELSE"
| FALLTHROUGH ->  "FALLTHROUGH"
| FOR ->  "FOR"
| FUNC ->  "FUNC"
| GO ->  "GO"
| GOTO ->  "GOTO"
| IF ->  "IF"
| IMPORT ->  "IMPORT"
| INTERFACE ->  "INTERFACE"
| MAP ->  "MAP"
| PACKAGE ->  "PACKAGE"
| RANGE ->  "RANGE"
| RETURN ->  "RETURN"
| SELECT ->  "SELECT"
| STRUCT ->  "STRUCT"
| SWITCH ->  "SWITCH"
| TYPE ->  "TYPE"
| VAR ->  "VAR"
| INT_TYP ->  "INT_TYP"
| FL_TYP ->  "FL_TYP"
| BOOL_TYP ->  "BOOL_TYP"
| RUNE_TYP ->  "RUNE_TYP"
| STR_TYP ->  "STR_TYP"
| PRINT ->  "PRINT"
| PRINTLN ->  "PRINTLN"
| APPEND  ->  "APPEND"



let rec loop_token_printer f lexbuf = 
	let tk = (f lexbuf) in
	print_token tk;
  match tk with
  | TEOF -> ()
  | _ -> loop_token_printer f lexbuf

let string_of_token_list tk_list = 
  "[" ^ 
  (List.fold_left
    (fun x y -> x ^ (string_of_token y) ^ "; " )
    ""
    tk_list
  ) ^ "]"
