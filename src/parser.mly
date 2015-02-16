%{
	exception NotImplemented
%}

%token TPLUS TMINUS TMULT TDIV TMOD TBITAND TBITOR TBITXOR TLSFT TRSFT TANOT
%token  TADDAS TSUBAS TMULAS TDIVAS TMODAS TANDAS TORAS TXORAS TLAS TRAS TANEQ
%token TAND TOR TREC TINC TDECR TEQ TLS TGR TASSIGN TNOT TNEQ TLSEQ TGREQ TCOLEQ
%token TTD TLPAR TRPAR TLBR TRBR TLCUR TRCUR TCOM TDOT TSEMCOL TCOL
%token TBLANKID
%token<string> TSTR TRWSTR TRUNE
%token TEOF
%token BREAK CASE CHAN CONST CONT DEFAULT DEFER ELSE FALLTHROUGH FOR FUNC GO
%token GOTO IF IMPORT INTERFACE MAP PACKAGE RANGE RETURN SELECT STRUCT SWITCH
%token TYPE VAR INT_TYP FL_TYP BOOL_TYP RUNE_TYP STR_TYP PRINT PRINTLN APPEND 
%token<string> ID
%token <string> DEC_INT OCTAL_INT HEX_INT 
%token <string> FLOAT64 

%left TOR
%left TAND
%left TEQ TNEQ TLS TLSEQ TGR TGREQ
%left TPLUS TMINUS TBITOR TBITXOR
%left TMULT TDIV TMOD TLSFT TRSFT TBITAND TANOT
%nonassoc UMINUS

%start <unit> program
(* %start <unit> expression *)
%%

program :  			
	| package_decl top_decl_list TEOF   { }

package_decl:
	| PACKAGE ID { }

top_decl_list :
	| list(top_decl) { }

top_decl :
	| declaration { }
	| func_decl { }

(*-----------*)

declaration :
	| var_decl { }
	| typ_decl { }

func_decl: 
	| FUNC ID signature func_body { }

(*-----------*)

var_decl:
	| VAR var_spec { }
	| VAR TLPAR var_spec TRPAR	{ }

typ_decl :
	| TYPE typ_spec { }
	| TYPE TLPAR typ_spec TRPAR	{ }

signature:
	| TLPAR param TRPAR typ	{ }
	| param	{ }

func_body:
	| stmt_list term_stmt { }

(*-----------*)

var_spec:
	| id_list typ 	{ }
	| id_list TASSIGN expr_list 	{ }
	| id_list typ TASSIGN expr_list { }

typ_spec:
	| ID typ 	{ }

param: {} 
	| pair_list { }

typ :
	| basic_typ  { }
	| slice_typ { }
	| array_typ { }
	| struct_typ { }

stmt_list: TEOF { }

term_stmt:
    | empty_stmt { }
    | expression_stmt { }
    | assign_stmt { }
    | declaration_stmt { }
    | shortvardecl_stmt { }
	| return_stmt { }
	| if_else_stmt { }
	| TEOF { }

(*-----------*)

id_list:
    | ID { }
	| id_list TCOM ID { }

expr_list:
    | expr { }
    | expr_list TCOM expr { }

pair_list:
	| pair_list id_list typ { }
	| id_list typ 	{ }

basic_typ :
	| INT_TYP	{ }
	| FL_TYP	{ }
	| BOOL_TYP	{ }
	| RUNE_TYP	{ }
	| STR_TYP 	{ }

slice_typ :
	| TLBR TRBR typ { }

array_typ:
	| TLBR int_literal TRBR typ { }

struct_typ:
	| STRUCT TLCUR pair_list TRCUR { }

empty_stmt: { }

expression_stmt:
    | expr { }

assign_stmt: 
	| expr assign_op expr { }
	| lvalue_list TASSIGN expr_list { }
	| blank_id TASSIGN expr { }

declaration_stmt:
    | declaration { }

shortvardecl_stmt:
    | id_list TCOLEQ expr_list { }

return_stmt: { }

if_else_stmt: { }

(*-----------*)

assign_op:
	| TADDAS | TSUBAS | TMULAS | TDIVAS | TMODAS | TANDAS
	| TORAS | TXORAS | TLAS | TRAS  { }

expr: { } (* TODO *)

lvalue_list:
    | lvalue { }
    | lvalue_list TCOM lvalue { }

lvalue:
    | ID { } (* TODO *)












(*
expression: identifier | literal | unary_exp | binary_exp | func_call | append_exp | type_cast_exp  { }

identifier: id_name = ID; { } 

literal: int_literal | float_literal | rune_literal | string_literal)

int_literal: decimal_lit | octal_lit | hex_lit { INT(something)}

decimal_lit: x = DEC_INT { DecInt(x) }
octal_lit: x = OCTAL_INT { OctalInt(x) }
hex_lit: x = HEX_INT { HexInt(x) }
*)

int_literal: {} 
blank_id: TBLANKID {}
%%
