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
%token<string> ID INT FLOAT64 

%left TOR
%left TAND
%left TEQ TNEQ TLS TLSEQ TGR TGREQ
%left TPLUS TMINUS TBITOR TBITXOR
%left TMULT TDIV TMOD TLSFT TRSFT TBITAND TANOT
%nonassoc UMINUS

%start <unit> program
%%

program :  			
	| top_decl_list stmt_list TEOF   { }

top_decl_list :
	| list(top_decl) { }

top_decl :
	| declaration { }
	| func_decl { }

declaration :
	| var_decl { }
	| typ_decl { }
	
var_decl:
	| VAR var_spec { }
	| VAR TLPAR var_spec TSEMCOL TRPAR	{ }

var_spec:
	| id_list typ 	{ }
	| id_list TASSIGN expr_list 	{ }
	| id_list typ TASSIGN expr_list { }


id_list: 	{ }

expr_list:  { }

typ_decl :
	| TYPE typ_spec { }
	| TYPE TLPAR typ_spec TSEMCOL TRPAR	{ }

typ_spec:
	| ID typ 	{ }

typ :
	| basic_typ  { }
	| slice_typ { }
	| array_typ { }
	| struct_typ { }
	| cust_typ { }

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
	| (* STRUCT TLCUR list(id_list typ) TSEMCOL TRCUR *){ }


cust_typ :  TEOF { }

func_decl: TEOF { }
stmt_list: TEOF { }
%%
