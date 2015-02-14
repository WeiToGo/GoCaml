%{
	exception NotImplemented
%}

%token TPLUS TMINUS TMULT TDIV TMOD TBITAND TBITOR TBITXOR TLSFT TRSFT TANOT
%token  TADDAS TSUBAS TMULAS TDIVAS TMODAS TANDAS TORAS TXORAS TLAS TRAS TANEQ
%token TAND TOR TREC TINC TDECR TEQ TLS TGR TASSIGN TNOT TNEQ TLSEQ TGREQ TCOLEQ
%token TTD TLPAR TRPAR TLBR TRBR TLCUR TRCUR TCOM TDOT TSEMCOL TCOL 
%token<string> TSTR TRWSTR TRUNE
%token TEOF
%token BREAK CASE CHAN CONST CONT DEFAULT DEFER ELSE FALLTHROUGH FOR FUNC GO
%token GOTO IF IMPORT INTERFACE MAP PACKAGE RANGE RETURN SELECT STRUCT SWITCH
%token TYPE VAR INT_TYP FL_TYP BOOL_TYP RUNE_TYP STR_TYP PRINT PRINTLN APPEND 
%token<string> ID 
%token <int> INT 
%token <float> FLOAT64 

%left TOR
%left TAND
%left TEQ TNEQ TLS TLSEQ TGR TGREQ
%left TPLUS TMINUS TBITOR TBITXOR
%left TMULT TDIV TMOD TLSFT TRSFT TBITAND TANOT
%nonassoc UMINUS

%start <unit> program
%%

program :  			
	| package_decl top_decl_list TEOF   { }

package_decl:
	| PACKAGE ID { }

id_list: 
	| list(ID) { }

pair_list:
	| pair_list id_list typ { }
	| id_list typ 	{ }

expr_list: { }

var_spec:
	| id_list typ 	{ }
	| id_list TASSIGN expr_list 	{ }
	| id_list typ TASSIGN expr_list { }

var_decl:
	| VAR var_spec { }
	| VAR TLPAR var_spec TRPAR	{ }

declaration :
	| var_decl { }
	| typ_decl { }

top_decl :
	| declaration { }
	| func_decl { }

top_decl_list :
	| list(top_decl) { }


typ_decl :
	| TYPE typ_spec { }
	| TYPE TLPAR typ_spec TRPAR	{ }

typ_spec:
	| ID typ 	{ }

typ :
	| basic_typ  { }
	| slice_typ { }
	| array_typ { }
	| struct_typ { }

basic_typ :
	| INT_TYP	{ }
	| FL_TYP	{ }
	| BOOL_TYP	{ }
	| RUNE_TYP	{ }
	| STR_TYP 	{ }

slice_typ :
	| TLBR TRBR typ { }

array_typ:
	| TLBR INT TRBR typ { }

struct_typ:
	| STRUCT TLCUR pair_list TRCUR { }


func_decl: 
	| FUNC ID signature func_body { }

signature:
	| TLPAR param TRPAR typ	{ }
	| param	{ }

param: {} 
	| pair_list { }


func_body:
	| stmt_list term_stmt { }

stmt_list: TEOF { }

term_stmt: 
	| return_stmt { }
	| if_else_stmt { } 
	| TEOF { }

return_stmt: {}
if_else_stmt: {}

%%
