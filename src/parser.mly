%{
  exception NotImplemented
  open Ast
%}

%token TPLUS TMINUS TMULT TDIV TMOD TBITAND TBITOR TCARET TLSFT TRSFT TANOT
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
  | package_decl TSEMCOL top_decl_list TEOF   { }

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
  | VAR TLPAR var_spec TSEMCOL TRPAR  { }

typ_decl :
  | TYPE typ_spec { }
  | TYPE TLPAR typ_spec TSEMCOL TRPAR { }

signature:
  | TLPAR param TRPAR typ { }
  | TLPAR param TRPAR     { }

func_body:
  | stmt_list term_stmt { }

(*-----------*)

var_spec:
  | id_list typ   { }
  | id_list TASSIGN expr_list   { }
  | id_list typ TASSIGN expr_list { }
  | TLPAR id_list typ TRPAR { }
  | TLPAR id_list TASSIGN expr_list TRPAR { }
  | TLPAR id_list typ TASSIGN expr_list TRPAR { }
  (* can't do TLPAR var_spec TRPAR because var (( ...)) is illegal. *)

typ_spec:
  | ID typ  { }

param: {} 
  | pair_list { }

typ :
  | basic_typ  { }
  | slice_typ { }
  | array_typ { }
  | struct_typ { }
  | ID { }

stmt_list:
    | stmt { }
    | stmt_list TSEMCOL stmt { }

stmt:
    | empty_stmt { }
    | expression_stmt { }
    | assign_stmt { }
    | declaration_stmt { }
    | shortvardecl_stmt { }
    | incdec_stmt { }
    | print_stmt { }
    | println_stmt { }
	  | return_stmt { }
    | if_stmt { }
    | switch_stmt { }
    | for_stmt { }
    | break_stmt { }
    | continue_stmt { }

term_stmt:
	| return_stmt { }

(*-----------*)

id_list:
    | ID { }
	| id_list TCOM ID { }

expr_list:
    | expr { }
    | expr_list TCOM expr { }

pair_list:
  | pair_list TCOM id_list typ { }
  | id_list typ   { }
  | (* empty *) { }

basic_typ :
  | INT_TYP { }
  | FL_TYP  { }
  | BOOL_TYP  { }
  | RUNE_TYP  { }
  | STR_TYP   { }

slice_typ :
  | TLBR TRBR typ { }
  | TLBR TRBR slice_typ { }

array_typ:
  | TLBR int_literal TRBR typ { }
  | TLBR int_literal TRBR array_typ { }

struct_typ:
  | STRUCT TLCUR pair_list TSEMCOL TRCUR { }

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

incdec_stmt:
    | lvalue TINC { }
    | lvalue TDECR { }

print_stmt:
    | PRINT TLPAR TRPAR { }
    | PRINT TLPAR expr_list TRPAR { }

println_stmt:
    | PRINTLN TLPAR TRPAR { }
    | PRINTLN TLPAR expr_list TRPAR { }

return_stmt:
    | RETURN { }
    | RETURN expr { }

if_stmt:
    | IF expr TLCUR stmt_list TRCUR { }
    | IF expr TLCUR stmt_list TRCUR ELSE TLCUR stmt_list TRCUR { }
    | IF expr TLCUR stmt_list TRCUR ELSE if_stmt { }

switch_stmt:
    | SWITCH TLCUR switch_clause_list TRCUR { }
    | SWITCH expr TLCUR switch_clause_list TRCUR { }
    | SWITCH simple_stmt TSEMCOL TLCUR switch_clause_list TRCUR { }
    | SWITCH simple_stmt TSEMCOL expr TLCUR switch_clause_list TRCUR { }

for_stmt:
    | FOR TLCUR stmt_list TRCUR { }
    | FOR expr TLCUR stmt_list TRCUR { }
    | FOR simple_stmt TSEMCOL expr TSEMCOL simple_stmt TLCUR stmt_list TRCUR { }

break_stmt:
    | BREAK { }

continue_stmt:
    | CONT { }

(*-----------*)

assign_op:
  | TADDAS | TSUBAS | TMULAS | TDIVAS | TMODAS | TANDAS
  | TORAS | TXORAS | TLAS | TRAS  { }

lvalue_list:
    | lvalue { }
    | lvalue_list TCOM lvalue { }

lvalue:
    | ID { } (* TODO *)
    | lvalue TLBR expr TRBR { } (* array indexing *)
    | lvalue TDOT ID { } (* struct field access *)

switch_clause_list:
    | switch_clause { }
    | switch_clause_list switch_clause { }


switch_clause:
    | DEFAULT TCOL stmt_list { }
    | CASE expr_list TCOL stmt_list { }

simple_stmt:
    | empty_stmt { }
    | expression_stmt { }
    | assign_stmt { }
    | shortvardecl_stmt { }
    | incdec_stmt { }







expr: ID | literal | unary_exp | binary_exp | append_exp | type_cast_exp  { }

literal: int_literal | float_literal| rune_literal | string_literal  { }

int_literal: decimal_lit | octal_lit | hex_lit { }

decimal_lit: x = DEC_INT { DecInt(x) }
octal_lit: x = OCTAL_INT { OctalInt(x) }
hex_lit: x = HEX_INT { HexInt(x) }

float_literal: x = FLOAT64 { FloatLit(x) }
rune_literal: x = TRUNE { RuneLit(x) }
string_literal: x = TRWSTR | x = TSTR { StringLit(x) }

unary_exp: primary_expression | unary_op unary_exp {}

primary_expression: function_call | index_exp | append_exp | type_cast_exp { }
unary_op:  TPLUS | TMINUS | TNOT | TCARET { }

function_call: ID; TLPAR; function_arguments; TRPAR { }
function_arguments: expr | function_arguments TCOM expr { }

index_exp: primary_expression TLBR expr TRBR { } 

append_exp: APPEND TLPAR ID TCOM expr TRPAR { }

type_cast_exp: castable_type TLPAR expr TRPAR {}
castable_type: INT_TYP | FL_TYP | RUNE_TYP | BOOL_TYP { } 

binary_exp: expr binary_op unary_exp { }

binary_op: TOR | TAND | rel_op | add_op | mul_op {}
rel_op: TEQ | TNEQ | TLS | TGR | TLSEQ | TGREQ { }
add_op: TPLUS | TMINUS | TBITOR | TCARET { }
mul_op: TMULT | TDIV | TMOD | TLSFT | TRSFT | TBITAND | TANOT { }

blank_id: TBLANKID {}


%%
