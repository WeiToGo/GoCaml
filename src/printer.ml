(* Helper printing functions for better testing *)
(* NOTE: This is NOT the pretty-printer *)

open Parser



let string_of_token tk = match tk with
| TPLUS ->  "TPLUS"
| TMINUS ->  "TMINUS"
| TMULT ->  "TMULT"
| TDIV ->  "TDIV"
| TMOD ->  "TMOD"
| TBITAND ->  "TBITAND"
| TBITOR ->  "TBITOR"
| TCARET ->  "TCARET"
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
| DEC_INT(s) -> "DEC_INT<" ^ s ^ ">"
| OCTAL_INT(s) -> "OCTAL_INT<" ^ s ^ ">"
| HEX_INT(s) -> "HEX_INT<" ^ s ^ ">" 
| TDECR -> "TDECR"
| FLOAT64(s) -> "FLOAT64<" ^ s ^ ">" 
| TBLANKID -> "TBLANKID"

let print_token oc tk = output_string oc ((string_of_token tk) ^ "\n")

let rec loop_token_printer f lexbuf out_channel =
	let tk = (f lexbuf) in
	print_token out_channel tk;
  match tk with
  | TEOF -> ()
  | _ -> loop_token_printer f lexbuf out_channel

let string_of_token_list tk_list = 
  "[" ^ 
  (List.fold_left
    (fun x y -> x ^ (string_of_token y) ^ "; " )
    ""
    tk_list
  ) ^ "]"
