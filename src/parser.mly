%{
  exception NotImplemented
  exception UnequalListLength
  exception ParsingError
  open Ast

  (* Both of these functions return things in reverse order 
    because of tail recursion. You will have to call List.rev on 
    the return value at some point *)
  let rec distribute xs y acc f = match xs with
  | [] -> acc
  | h :: t -> distribute t y ((f h y) :: acc) f

  let rec list_zip xs ys acc f = match xs, ys with
  | [], [] -> acc
  | (h :: t), (h' :: t') -> list_zip t t' ((f h h') :: acc) f
  | _ -> raise UnequalListLength

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
%left TPLUS TMINUS TBITOR TCARET
%left TMULT TDIV TMOD TLSFT TRSFT TBITAND TANOT
%nonassoc uop
%nonassoc fun_parens

%start<Ast.expression> program

%%

program :       
  (*| package_decl TSEMCOL top_decl_list TEOF  { } *)
  | expr TSEMCOL { $1 }

package_decl:
  | PACKAGE ID { }


top_decl_list :
  | (* empty *)   { }
  | top_decl_list top_decl TSEMCOL { } 


top_decl :
  | declaration { } 
  | func_decl { } 

(*-----------*)

declaration :
  | var_decl { }
  | typ_decl {  }

func_decl: 
  | FUNC ID func_signature TLCUR func_body TRCUR { } 

(*-----------*)

var_decl:
  | VAR var_spec { }
  | VAR TLPAR var_spec_list TRPAR  { }

typ_decl :
  | TYPE typ_spec { }
  | TYPE TLPAR typ_spec_list TRPAR {  }  (* This might cause weird errors. Check back *)

func_signature:
  | TLPAR args_list TRPAR typ {  }
  | TLPAR args_list TRPAR {  } 


args_list: 
  | (* empty *) { }
  | args_list TCOM id_list typ 
    { }
  | id_list typ 
    {  } 

func_body:
    | stmt_list stmt {  }

(*-----------*)

var_spec:
  | id_list typ
    {   }
  | id_list TASSIGN expr_list
    {    }
  | id_list typ TASSIGN expr_list 
    {   }

var_spec_list:
  | (* empty *) { }
  | var_spec_list var_spec TSEMCOL { } 

typ_spec:
  | ID typ  {  }

typ_spec_list:
  | (* empty *) { }
  | typ_spec_list typ_spec TSEMCOL { }

typ:
  | basic_typ  { }
  | slice_typ {  }
  | array_typ {  }
  | struct_typ {  }
  | ID { }

stmt_list:
    | (* empty *) { }
    | stmt_list stmt TSEMCOL { }

stmt:
    | empty_stmt {  }
    | expression_stmt {  }
    | assign_stmt { }
    | declaration_stmt {  }
    | shortvardecl_stmt {  }
    | incdec_stmt { }
    | print_stmt {  }
    | println_stmt {  }
    | return_stmt {  }
    | if_stmt {  } 
    | switch_stmt {  }
    | for_stmt { }
    | break_stmt {  }
    | continue_stmt {  }
    | block_stmt { }
(*-----------*)

id_list:
  | ID {  }
  | id_list TCOM ID {  }

expr_list:
    | expr { }
    | expr_list TCOM expr { }

basic_typ :
  | INT_TYP {  }
  | FL_TYP  {  }
  | BOOL_TYP  {  }
  | RUNE_TYP  {  }
  | STR_TYP   {  }

slice_typ :
  | TLBR TRBR typ {  }

array_typ:
  | TLBR int_literal TRBR typ { }

struct_typ:
  | STRUCT TLCUR field_decl_list TRCUR {  }

field_decl_list: 
  | (* empty *) {  }
  | field_decl_list field_decl TSEMCOL { }

field_decl: 
  | id_list typ 
    {   }


empty_stmt: 
  | (* empty *) {  }

expression_stmt:
    | expr {  }

assign_stmt:
  | lvalue single_op expr {  }
  | lvalue_list TASSIGN expr_list
     {  }
  | blank_id TASSIGN expr { }

declaration_stmt:
    | declaration 
        {    }

shortvardecl_stmt:
    | lvalue_list TCOLEQ expr_list   (* make sure lvalue is ID only? *)
      {    }

incdec_stmt:
    | lvalue TINC 
        {     }
    | lvalue TDECR
        {   }


print_stmt:
    | PRINT TLPAR TRPAR { }
    | PRINT TLPAR expr_list TRPAR { }

println_stmt:
    | PRINTLN TLPAR TRPAR {  }
    | PRINTLN TLPAR expr_list TRPAR {  }

return_stmt:
    | RETURN {  }
    | RETURN expr {  }




if_stmt:
    | IF expr TLCUR stmt_list TRCUR {  }
    | IF expr TLCUR stmt_list TRCUR ELSE TLCUR stmt_list TRCUR { }
    | IF expr TLCUR stmt_list TRCUR ELSE if_stmt { }
    | IF simple_stmt TSEMCOL expr TLCUR stmt_list TRCUR {  }
    | IF simple_stmt TSEMCOL expr TLCUR stmt_list TRCUR ELSE TLCUR stmt_list TRCUR {  }
    | IF simple_stmt TSEMCOL expr TLCUR stmt_list TRCUR ELSE if_stmt {  }

switch_stmt:
    | SWITCH TLCUR switch_clause_list TRCUR {  }
    | SWITCH expr TLCUR switch_clause_list TRCUR { }
    | SWITCH simple_stmt TSEMCOL TLCUR switch_clause_list TRCUR { }
    | SWITCH simple_stmt TSEMCOL expr TLCUR switch_clause_list TRCUR { }

for_stmt:
    | FOR TLCUR stmt_list TRCUR { }
    | FOR expr TLCUR stmt_list TRCUR {  }
    | FOR simple_stmt TSEMCOL option(expr) TSEMCOL simple_stmt TLCUR stmt_list TRCUR 
        { }

break_stmt:
    | BREAK {  }

continue_stmt:
    | CONT {  }

block_stmt:
    | TLCUR stmt_list TRCUR { }
(*-----------*)


lvalue_list:
    | lvalue_list TCOM lvalue { }
    | lvalue { }

lvalue:   
    | ID {  }
    | primary_expression TLBR expr TRBR { (*make sure only ID,index_exp.. *) } (* array indexing *)
    | primary_expression TDOT ID { } (* struct field access *)


switch_clause_list:
    | switch_clause {  }
    | switch_clause_list switch_clause { }


switch_clause:
    | DEFAULT TCOL stmt_list { }
    | CASE expr_list TCOL stmt_list { }

simple_stmt:
    | empty_stmt {  }
    | expression_stmt {  }
    | assign_stmt {  }
    | shortvardecl_stmt {  }
    | incdec_stmt {  }


(* ----- expressions ----- *)

expr:
    | unary_exp { $1 } 
    | binary_exp { LiteralExp(FloatLit("testest")) }

literal: 
    | int_literal { IntLit $1 } 
    | float_literal { FloatLit($1) } 
    | rune_literal { RuneLit($1) } 
    | string_literal  { StringLit($1) }

int_literal: 
    | DEC_INT { DecInt $1  }
    | OCTAL_INT { OctalInt $1 }
    | HEX_INT { HexInt $1 }

float_literal: FLOAT64 { $1 }

rune_literal: TRUNE { $1 }

string_literal: 
    | TRWSTR { $1 } 
    | TSTR { $1 }

unary_exp:
  | TLPAR expr TRPAR { $2 }
  | primary_expression { $1 }
  | unary_op unary_exp %prec uop { UnaryExp($1, $2) } 

primary_expression: 
  | ID { IdExp(IdName($1)) } 
  | literal { LiteralExp $1 } 
  | function_call { $1 } 
  | index_exp { $1 } 
  | append_exp { $1 } 
  | select_exp  { $1 }
  | type_cast_exp { UnaryExp(UNot, IdExp(IdName("testtest"))) }

unary_op:
  | TPLUS { UPlus } 
  | TMINUS { UMinus } 
  | TNOT { UNot } 
  | TCARET { UCaret } 


function_call:
  | unary_exp TLPAR function_arguments TRPAR %prec fun_parens
      { FunctionCallExp($1, $3)}

function_arguments: 
  | (* empty *) { [] }
  | non_empty_function_arguments { List.rev $1 }

non_empty_function_arguments:
  | expr { [ $1 ] }
  | non_empty_function_arguments TCOM expr { $3::$1 }
 
index_exp: 
  | primary_expression TLBR expr TRBR 
      { IndexExp($1, $3)} 

append_exp:
  | APPEND TLPAR ID TCOM expr TRPAR   { AppendExp(IdName($3), $5) }

select_exp: 
  | primary_expression TDOT ID 
      { SelectExp($1, IdName($3)) }

type_cast_exp:
  | castable_type TLPAR expr TRPAR 
      {  }

castable_type: 
  | INT_TYP {  }
  | FL_TYP {  } 
  | RUNE_TYP {  }
  | BOOL_TYP {  } 
 
binary_exp:
  | expr binary_op expr {  }

%inline binary_op:
  | TOR { }
  | TAND { }
  | rel_op { }
  | add_op { }
  | mul_op { }

%inline rel_op: 
  | TEQ {  } 
  | TNEQ {  }
  | TLS {  }
  | TGR {  }
  | TLSEQ {  }
  | TGREQ {  }

%inline add_op: 
  | TPLUS {  }  
  | TMINUS {  }
  | TBITOR {  } 
  | TCARET {  } 

%inline mul_op: 
  | TMULT {  } 
  | TDIV {  }
  | TMOD {  } 
  | TLSFT {  } 
  | TRSFT {  }
  | TBITAND {  } 
  | TANOT {  } 

%inline single_op:
  | TADDAS { } 
  | TSUBAS { } 
  | TMULAS { } 
  | TDIVAS { } 
  | TMODAS { } 
  | TANDAS { } 
  | TORAS  { } 
  | TXORAS { } 
  | TLAS   { } 
  | TRAS   { }
  | TANEQ  { }

blank_id: TBLANKID { }

%%
