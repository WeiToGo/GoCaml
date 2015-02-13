%start <unit> program
%%

program :  			
	| top_decl_list stmt_list EOF   { }

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

id_list:

expr_list:

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
	| KINT_TYP	{ }
	| KFL_TYP	{ }
	| KBOOL_TYP	{ }
	| KRUNE_TYP	{ }
	| KSTR_TYP 	{ }

slice_typ :
	| TLBR TRBR typ { }

array_typ:
	| TLBR INT TRBR typ { }
	
struct_typ :
	| STRUCT TLCUR list(id_list typ) TSEMCOL TRCUR { }

cust_typ :
	| { }

%%
