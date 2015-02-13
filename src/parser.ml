%start <unit> program
%%

program :  			
	| decl_list stmt_list EOF   { }

decl_list :
	| list(declaration) { }

declaration :
	| var_decl { }
	| typ_decl { }
	| func_decl { }
	
typ :
	| basic_typ  { }
	| slice_typ { }
	| array_typ { }
	| struct_typ { }
	| cust_typ { }

%%
