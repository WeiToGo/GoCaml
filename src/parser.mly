%{
  exception NotImplemented
  exception UnequalListLength
  exception ParsingError
  open Ast
  (* THIS FILE WILL NOT COMPILE. *)

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

%start<Ast.program> program

%%

program :       
  | package_decl TSEMCOL top_decl_list TEOF  { Program($1, List.rev $3)} 

package_decl:
  | PACKAGE ID { Package($2) }

top_decl_list :
  | (* empty *)   { [] }
  | top_decl_list top_decl TSEMCOL { $2 :: $1 }  

top_decl :
  | declaration { $1 } 
  | func_decl { FunctionDecl $1 }

(*-----------*)

declaration :
  | var_decl { VarDeclBlock $1 }
  | typ_decl { TypeDeclBlock $1 }

func_decl: 
  | FUNC ID func_signature TLCUR func_body TRCUR { Function(IdName($2), $3, $5) } 

(*-----------*)

var_decl:
  | VAR var_spec { [ $2 ] }
  | VAR TLPAR var_spec_list TRPAR  { List.rev $3 }

typ_decl :
  | TYPE typ_spec TSEMCOL { [ $2 ] }
  | TYPE TLPAR typ_spec_list TRPAR { List.rev $3 }  (* This might cause weird errors. Check back *)

func_signature:
  | TLPAR args_list TRPAR typ { FunctionSig($2, Some($4)) }
  | TLPAR args_list TRPAR { FunctionSig($2, None) } 


args_list: 
  | (* empty *) { [] }
  | args_list TCOM id_list typ 
    {
      let res = distribute (List.rev $3) $4 [] (fun x y -> FunctionArg(x, y))
      in $1 @ (List.rev res)
    }
  | id_list typ 
    {
      let res = distribute (List.rev $1) $2 [] (fun x y -> FunctionArg(x, y))
      in List.rev res 
     } 

func_body:
  | stmt_list { List.rev $1 }

(*-----------*)

var_spec:
  | id_list typ
    { 
      let res = distribute $1 $2 [] (fun x y -> (SingleVarDecl (x, Some(y), None))) in 
      MultipleVarDecl(List.rev res)
    }
  | id_list TASSIGN expr_list
    {
      try 
        let res = list_zip $1 $3 [] (fun x y -> SingleVarDecl( x, None, Some(y)))
        in MultipleVarDecl(List.rev res)
      with UnequalListLength -> raise ParsingError
    }
  | id_list typ TASSIGN expr_list 
    { 
      let res = list_zip $1 $4 [] (fun x y -> SingleVarDecl(x, Some($2), Some(y)))
      in MultipleVarDecl(List.rev res)
    }

var_spec_list:
  | (* empty *) { [] }
  | var_spec_list var_spec TSEMCOL { $2 :: $1 } 

typ_spec:
  | ID typ  { SingleTypeDecl(IdName($1), $2) }

typ_spec_list:
  | (* empty *) { [] }
  | typ_spec_list typ_spec TSEMCOL { $2 :: $1 }

typ:
  | basic_typ  { BasicType $1 }
  | slice_typ { $1 }
  | array_typ { $1 }
  | struct_typ { $1 }
  | ID { CustomType(IdName($1)) }


stmt_list:
    | (* empty *) { [ ] }
    | stmt_list stmt TSEMCOL { $2 :: $1 }

stmt:
    | empty_stmt { $1 }
    | expression_stmt { $1 }
    | assign_stmt { $1 }
    | declaration_stmt { $1 }
    | shortvardecl_stmt { $1 }
    | incdec_stmt { $1 }
    | print_stmt { $1 }
    | println_stmt { $1 }
    | return_stmt { $1 }
    | if_stmt { $1 } 
    | switch_stmt { $1 }
    | for_stmt { $1 }
    | break_stmt { $1 }
    | continue_stmt { $1 }

(*-----------*)

id_list:
  | ID { [IdName($1)] }
  | id_list TCOM ID { IdName($3) :: $1 }

expr_list:
    | expr { [$1] }
    | expr_list TCOM expr { $3 :: $1}

basic_typ :
  | INT_TYP { IntType }
  | FL_TYP  { FloatType }
  | BOOL_TYP  { BoolType }
  | RUNE_TYP  { RuneType }
  | STR_TYP   { StringType }

slice_typ :
  | TLBR TRBR typ { SliceType $3 }

array_typ:
  | TLBR int_literal TRBR typ { ArrayType($2, $4) }

struct_typ:
  | STRUCT TLCUR field_decl_list TRCUR { StructType( List.rev $3) }

field_decl_list: 
  | (* empty *) { [] }
  | field_decl_list field_decl TSEMCOL { $2 :: $1 }

field_decl: 
  | id_list typ 
    {
      let res = distribute $1 $2 [] (fun x y -> StructField(x, y))
      in StructFieldDecl(List.rev res)
    }

empty_stmt: 
  | (* empty *) { EmptyStatement }

expression_stmt:
    | expr { ExpressionStatement $1 }

assign_stmt:
  | lvalue single_op expr { SingleAssign($1,$2,$3) } (* TO ADD to ast *)
  | lvalue_list TASSIGN expr_list
     {
        let reversed_lvalue = List.rev $1 in
        let res = list_zip reversed_lvalue $3 [] (fun x y -> (x, y)) in
        AssignmentStatement(List.rev res)
     }
  | blank_id TASSIGN expr { AssignmentStatement([(LId(BlankID), $3)]) }


declaration_stmt:
    | declaration 
        { match $1 with 
            VarDeclBlock(x) -> VarDeclBlockStatement(x)
          | TypeDeclBlock(x) -> TypeDeclBlockStatement(x) 
        }

shortvardecl_stmt:
    | lvalue_list TCOLEQ expr_list   (* make sure lvalue is ID only? *)
(*    | id_list TCOLEQ expr_list    WARNING!!!! *)
      {
        let res = list_zip $1 $3 [] (fun x y -> SingleVarDecl(x, None, Some(y)))
        in VarDeclBlockStatement([ MultipleVarDecl (List.rev res) ] )
      }

incdec_stmt:
    | lvalue TINC 
        { 
          let lexp = exp_of_lvalue $1 in
          AssignmentStatement([($1, BinaryExp(BinPlus, lexp, LiteralExp(IntLit(DecInt("1")))))])
        }
    | lvalue TDECR
        { 
          let lexp = exp_of_lvalue $1 in
          AssignmentStatement([($1, BinaryExp(BinMinus, lexp, LiteralExp(IntLit(DecInt("1")))))])
        }


print_stmt:
    | PRINT TLPAR TRPAR { PrintStatement([])}
    | PRINT TLPAR expr_list TRPAR { PrintStatement(List.rev $3 )}

println_stmt:
    | PRINTLN TLPAR TRPAR { PrintlnStatement([]) }
    | PRINTLN TLPAR expr_list TRPAR { PrintlnStatement(List.rev $3) }

return_stmt:
    | RETURN { ReturnStatement(None) }
    | RETURN expr { ReturnStatement(Some($2)) }

if_stmt:
    | IF expr TLCUR stmt_list TRCUR { IfStatement(None, $2, List.rev $4, None) }
    | IF expr TLCUR stmt_list TRCUR ELSE TLCUR stmt_list TRCUR { IfStatement(None, $2, List.rev $4, Some(List.rev $8))}
    | IF expr TLCUR stmt_list TRCUR ELSE if_stmt { IfStatement(None, $2, List.rev $4, Some([$7]))}
    | IF simple_stmt TSEMCOL expr TLCUR stmt_list TRCUR { IfStatement(Some($2), $4, List.rev $6, None) }
    | IF simple_stmt TSEMCOL expr TLCUR stmt_list TRCUR ELSE TLCUR stmt_list TRCUR { IfStatement(Some($2), $4, List.rev $6, Some($10)) }
    | IF simple_stmt TSEMCOL expr TLCUR stmt_list TRCUR ELSE if_stmt { IfStatement(Some($2), $4, List.rev $6, Some([$9])) }

switch_stmt:
    | SWITCH TLCUR switch_clause_list TRCUR { SwitchStatement(None,IdExp(IdName("true")), List.rev $3)}
    | SWITCH expr TLCUR switch_clause_list TRCUR { SwitchStatement(None, $2, List.rev $4)}
    | SWITCH simple_stmt TSEMCOL TLCUR switch_clause_list TRCUR { SwitchStatement(Some($2),IdExp(IdName("true")), List.rev $5) }
    | SWITCH simple_stmt TSEMCOL expr TLCUR switch_clause_list TRCUR { SwitchStatement(Some($2), $4, List.rev $6) }

for_stmt:
    | FOR TLCUR stmt_list TRCUR { ForStatement(None, IdExp(IdName("true")), None, List.rev $3 )}
    | FOR expr TLCUR stmt_list TRCUR { ForStatement(None, $2, None, List.rev $4 ) }
    | FOR simple_stmt TSEMCOL expr TSEMCOL simple_stmt TLCUR stmt_list TRCUR 
        { ForStatement(Some($2), $4, Some($6), $8) }

break_stmt:
    | BREAK { BreakStatement }

continue_stmt:
    | CONT { ContinueStatement }

(*-----------*)

lvalue_list:
    | lvalue_list TCOM lvalue { $3 :: $1}
    | lvalue { [ $1 ]}

lvalue:   (*  If conflict, fix this *)
    | ID { LId(IdName($1)) }
    (*make sure primary_exp is only ID,index_exp.. *)
    | primary_expression TLBR expr TRBR { LIndex($1, $3) } (* array indexing *)
    | primary_expression TDOT ID { LSelector($1, IdName($3)) } (* struct field access *)

switch_clause_list:
    | switch_clause { [$1] }
    | switch_clause_list switch_clause { $2::$1}


switch_clause:
    | DEFAULT TCOL stmt_list { DefaultCase (List.rev $3)}
    | CASE expr_list TCOL stmt_list { SwitchCase ($2, List.rev $4)}

simple_stmt:
    | empty_stmt { $1 }
    | expression_stmt { $1 }
    | assign_stmt { $1 }
    | shortvardecl_stmt { $1 }
    | incdec_stmt { $1 }

expr: 
    | unary_exp { $1 } 
    | binary_exp { $1 }

literal: 
    | int_literal { IntLit $1 } 
    | float_literal { $1 } 
    | rune_literal { $1 } 
    | string_literal  { $1 }

int_literal: 
    | DEC_INT { DecInt($1) }
    | OCTAL_INT { OctalInt($1) }
    | HEX_INT { HexInt($1) }

float_literal: FLOAT64 { FloatLit($1) }

rune_literal: TRUNE { RuneLit($1) }

string_literal: 
    | TRWSTR { StringLit($1) } 
    | TSTR { StringLit($1) }

unary_exp: 
  | primary_expression { $1 }
  | unary_op unary_exp { UnaryExp($1, $2) }

primary_expression: 
  | ID { IdExp ( IdName($1)) } 
  | literal { LiteralExp $1} 
  | function_call { $1 } 
  | index_exp { $1 } 
  | append_exp { $1 } 
  | select_exp  { $1 }
  | type_cast_exp { $1 }

unary_op:
  | TPLUS { UPlus } 
  | TMINUS { UMinus } 
  | TNOT { UNot } 
  | TCARET { UCaret } 

function_call:
  | ID TLPAR function_arguments TRPAR 
      { FunctionCallExp(IdName($1), $3 )}

function_arguments: 
  | (* empty *) { [] }
  | non_empty_function_arguments { List.rev $1 }

non_empty_function_arguments:
  | expr { [ $1 ]  }
  | function_arguments TCOM expr { $3 :: $1 }

index_exp: 
  | primary_expression TLBR expr TRBR 
      { IndexExp($1, $3) } 

append_exp:
  | APPEND TLPAR ID TCOM expr TRPAR
      { AppendExp(IdName($3), $5) }

select_exp: 
  | primary_expression TDOT ID 
      { SelectExp($1, IdName($3)) }

type_cast_exp:
  | castable_type TLPAR expr TRPAR 
      { TypeCastExp($1, $3) }

castable_type: 
  | INT_TYP { BasicType(IntType) }
  | FL_TYP { BasicType(FloatType) } 
  | RUNE_TYP { BasicType(RuneType) }
  | BOOL_TYP { BasicType(BoolType) } 
 
binary_exp:
  | e1 = expr; op = binary_op; e2 = expr { BinaryExp(op, e1, e2) }

%inline binary_op:
  | TOR { BinOr}
  | TAND {BinAnd}
  | op = rel_op { op }
  | op = add_op { op }
  | op = mul_op { op }

%inline rel_op: 
  | TEQ { BinEq } 
  | TNEQ { BinNotEq }
  | TLS { BinLess }
  | TGR { BinGreater }
  | TLSEQ { BinLessEq }
  | TGREQ { BinGreaterEq }

%inline add_op: 
  | TPLUS { BinPlus }  
  | TMINUS { BinMinus }
  | TBITOR { BinBitOr } 
  | TCARET { BinBitXor } 

%inline mul_op: 
  | TMULT { BinMult } 
  | TDIV { BinDiv }
  | TMOD { BinMod } 
  | TLSFT { BinShiftLeft } 
  | TRSFT { BinShiftRight }
  | TBITAND { BinBitAnd } 
  | TANOT { BinBitAndNot } 

%inline single_op:
  | TADDAS { SinADD } 
  | TSUBAS { SinSUB } 
  | TMULAS { SinMul } 
  | TDIVAS { SinDiv } 
  | TMODAS { SinMod } 
  | TANDAS { SinAnd} 
  | TORAS  { SinOr} 
  | TXORAS { SinXor} 
  | TLAS   { SinLas} 
  | TRAS   { SinRas}
  | TANEQ  { SinAneq}

blank_id: TBLANKID { BlankID }

%%
