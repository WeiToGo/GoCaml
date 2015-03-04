%{
  exception NotImplemented
  exception UnequalListLength
  exception ParsingError
  exception NonIDExpr
  open Ast
  open Lexing

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
%token TAND TOR TINC TDECR TEQ TLS TGR TASSIGN TNOT TNEQ TLSEQ TGREQ TCOLEQ
%token TLPAR TRPAR TLBR TRBR TLCUR TRCUR TCOM TDOT TSEMCOL TCOL
%token TBLANKID
%token<string> TSTR TRWSTR TRUNE
%token TEOF
%token BREAK CASE CONT DEFAULT ELSE FOR FUNC
%token IF PACKAGE RETURN STRUCT SWITCH
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
%nonassoc TLPAR TLBR TDOT


%start<Ast.program> program

%%

program :       
  | package_decl TSEMCOL top_decl_list TEOF  { Program($1, List.rev $3) }

package_decl:
  | PACKAGE ID { Package $2 }


top_decl_list :
  | (* empty *)   { [] }
  | top_decl_list top_decl TSEMCOL { $2 :: $1 } 


top_decl :
  | declaration { $1 } 
  | func_decl { $1 } 

(*-----------*)

declaration :
  | var_decl { VarDeclBlock $1 }
  | typ_decl { TypeDeclBlock $1 }

func_decl: 
  | FUNC ID func_signature TLCUR stmt_list TRCUR 
      { FunctionDecl(IdName($2), $3, List.rev $5) } 

(*-----------*)

var_decl:
  | VAR var_spec { [$2] }
  | VAR TLPAR var_spec_list TRPAR  { List.rev $3 }

typ_decl :
  | TYPE typ_spec { [$2] }
  | TYPE TLPAR typ_spec_list TRPAR { List.rev $3 }

func_signature:
  | TLPAR args_list TRPAR typ 
      { FunctionSig($2, Some $4) }
  | TLPAR args_list TRPAR 
      { FunctionSig($2, None) } 


args_list: (* This is ugly. The list append is not ideal *)
  | (* empty *) 
      { [] }
  | args_list TCOM id_list typ 
    { $1 @ (distribute $3 $4 [] (fun x y -> FunctionArg(x, y)))}
  | id_list typ 
    { distribute $1 $2 [] (fun x y -> FunctionArg(x, y)) } 


(*-----------*)

var_spec:   (* Returns multivardecl *)
  | id_list typ
    { MultipleVarDecl(distribute $1 $2 [] (fun x y -> SingleVarDecl(x, Some y, None))) }
  | id_list TASSIGN expr_list
    {  MultipleVarDecl(list_zip $1 $3 [] (fun x y -> SingleVarDecl(x, None, Some y)))  }
  | id_list typ TASSIGN expr_list 
    { MultipleVarDecl(list_zip $1 $4 [] (fun x y -> SingleVarDecl(x, Some $2, Some y))) }

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
  | func_typ { $1 } 
  | ID { CustomType(IdName($1)) }

func_typ:
  | FUNC TLPAR typ_list TRPAR option(typ)
    { FunctionType(List.rev $3, $5)}

typ_list:
  | (* empty *) { [] }
  | non_empty_typ_list { $1 }

non_empty_typ_list:
  | typ { [$1] }
  | non_empty_typ_list TCOM typ { $3 :: $1 }

stmt_list:
    | (* empty *) { [] }
    | stmt_list stmt TSEMCOL { $2 :: $1 }

stmt:
    | empty_stmt { LinedStatement($startpos.pos_lnum, $1) }
    | expression_stmt { LinedStatement($startpos.pos_lnum, $1) }
    | assign_stmt { LinedStatement($startpos.pos_lnum, $1) }
    | declaration_stmt { LinedStatement($startpos.pos_lnum, $1)  }
    | shortvardecl_stmt { LinedStatement($startpos.pos_lnum, $1)  }
    | incdec_stmt { LinedStatement($startpos.pos_lnum, $1) }
    | print_stmt { LinedStatement($startpos.pos_lnum, $1)  }
    | println_stmt { LinedStatement($startpos.pos_lnum, $1)  }
    | return_stmt { LinedStatement($startpos.pos_lnum, $1) }
    | if_stmt { LinedStatement($startpos.pos_lnum, $1) } 
    | switch_stmt { LinedStatement($startpos.pos_lnum, $1)  }
    | for_stmt { LinedStatement($startpos.pos_lnum, $1) }
    | break_stmt { LinedStatement($startpos.pos_lnum, $1)  }
    | continue_stmt { LinedStatement($startpos.pos_lnum, $1)  }
    | block_stmt { LinedStatement($startpos.pos_lnum, $1) }
(*-----------*)

id_list: (* Non-empty *)
  | identifier { [$1] }
  | id_list TCOM identifier { $3 :: $1 }

basic_typ :
  | INT_TYP { IntType }
  | FL_TYP  { FloatType }
  | BOOL_TYP  { BoolType }
  | RUNE_TYP  { RuneType }
  | STR_TYP   { StringType }

slice_typ :
  | TLBR TRBR typ { SliceType $3 }

array_typ:
  | TLBR int_literal TRBR typ { ArrayType($2, $4)}

struct_typ:
  | STRUCT TLCUR field_decl_list TRCUR { StructType(List.rev $3) }

field_decl_list: 
  | (* empty *) { [] }
  | field_decl_list field_decl TSEMCOL { $2 :: $1 }

field_decl: (* Returns multi_struct_field_decl *)
  | id_list typ 
    { MultipleStructFieldDecl(distribute $1 $2 [] (fun x y -> SingleStructFieldDecl(x, y))) }


empty_stmt: 
  | (* empty *) { EmptyStatement }

expression_stmt:
    | expr { ExpressionStatement($1) }

assign_stmt:
  | lv = primary_expression; op = single_op; exp = expr 
    { let binop_of_assignment_op op = match op with
      |SinADD -> BinPlus
      |SinSUB -> BinMinus
      |SinMul -> BinMult
      |SinDiv -> BinDiv
      |SinMod -> BinMod
      |SinAnd -> BinBitAnd
      |SinOr -> BinBitOr
      |SinXor -> BinBitXor
      |SinLas -> BinShiftLeft
      |SinRas -> BinShiftRight
      |SinAneq -> BinBitAndNot
      in
      AssignmentStatement([(lv, BinaryExp(binop_of_assignment_op op, lv, exp))])
     }
  | expr_list TASSIGN expr_list
     { AssignmentStatement(list_zip $1 $3 []  (fun x y -> (x,y)) ) }

expr_list:
    | expr_list TCOM expr { $3 :: $1 }
    | expr { [ $1 ] }

declaration_stmt:
    | var_decl 
        { VarDeclBlockStatement $1 }
    | typ_decl
        { TypeDeclBlockStatement $1 }

shortvardecl_stmt:  
    | expr_list TCOLEQ expr_list
      {
       let id_of_expr xp = match xp with
        | IdExp(id) -> id
        | _ -> raise NonIDExpr 
        in
        let id_list = List.map id_of_expr $1 in
        ShortVarDeclStatement(
            list_zip id_list $3 [] (fun x y -> ShortVarDecl(x, y) )
        )
      }
 
incdec_stmt:
    | primary_expression TINC 
        { AssignmentStatement([($1, BinaryExp(BinPlus, $1, 
            LiteralExp(IntLit(DecInt("1")))
          ))])
        }
    | primary_expression TDECR
        { AssignmentStatement([($1, BinaryExp(BinMinus, $1,
            LiteralExp(IntLit(DecInt("1")))
          ))])    
        }


print_stmt:
    | PRINT TLPAR TRPAR { PrintStatement([]) }
    | PRINT TLPAR expr_list TRPAR { PrintStatement(List.rev $3) }

println_stmt:
    | PRINTLN TLPAR TRPAR { PrintlnStatement([]) }
    | PRINTLN TLPAR expr_list TRPAR { PrintlnStatement(List.rev $3) }

return_stmt:
    | RETURN { ReturnStatement(None) }
    | RETURN expr { ReturnStatement(Some $2) }




if_stmt:
    | IF expr TLCUR stmt_list TRCUR 
        { IfStatement(None, $2, $4, None) }
    | IF expr TLCUR stmt_list TRCUR ELSE TLCUR stmt_list TRCUR 
        { IfStatement(None, $2, List.rev $4, Some (List.rev $8)) }
    | IF expr TLCUR stmt_list TRCUR ELSE if_stmt 
        { IfStatement(None, $2, List.rev $4,
           Some [LinedStatement($startpos.pos_lnum, $7)]) }
    | IF simple_stmt TSEMCOL expr TLCUR stmt_list TRCUR 
        { IfStatement(Some $2, $4, List.rev $6, None) }
    | IF simple_stmt TSEMCOL expr TLCUR stmt_list TRCUR ELSE TLCUR stmt_list TRCUR 
        { IfStatement(Some $2, $4, List.rev $6, Some (List.rev $10)) }
    | IF simple_stmt TSEMCOL expr TLCUR stmt_list TRCUR ELSE if_stmt 
        { IfStatement(Some $2, $4, List.rev $6,
           Some [LinedStatement($startpos.pos_lnum, $9)]) }

switch_stmt:
    | SWITCH TLCUR switch_clause_list TRCUR 
        { SwitchStatement(None, IdExp(IdName("true")), List.rev $3) }
    | SWITCH expr TLCUR switch_clause_list TRCUR 
        { SwitchStatement(None, $2, List.rev $4) }
    | SWITCH simple_stmt TSEMCOL TLCUR switch_clause_list TRCUR 
        { SwitchStatement(Some $2, IdExp(IdName("true")), List.rev $5)}
    | SWITCH simple_stmt TSEMCOL expr TLCUR switch_clause_list TRCUR 
        { SwitchStatement(Some $2, $4, List.rev $6) } 

for_stmt:
    | FOR TLCUR stmt_list TRCUR { ForStatement(None, IdExp(IdName("true")), None, List.rev $3) }
    | FOR expr TLCUR stmt_list TRCUR { ForStatement(None, $2, None, List.rev $4) }
    | FOR simple_stmt TSEMCOL expr TSEMCOL simple_stmt TLCUR stmt_list TRCUR 
        { ForStatement(Some $2, $4, Some $6, List.rev $8) } 
    | FOR simple_stmt TSEMCOL TSEMCOL simple_stmt TLCUR stmt_list TRCUR 
        { ForStatement(Some $2, IdExp(IdName("true")), Some $5, List.rev $7) }

break_stmt:
    | BREAK { BreakStatement }

continue_stmt:
    | CONT { ContinueStatement }

block_stmt:
    | TLCUR stmt_list TRCUR { BlockStatement(List.rev $2) }
(*-----------*)





switch_clause_list:
    | (* empty *) { [] }
    | switch_clause_list switch_clause { $2 :: $1 }


switch_clause:
    | DEFAULT TCOL stmt_list 
        { DefaultCase (List.rev $3) }
    | CASE expr_list TCOL stmt_list 
        { SwitchCase (List.rev $2, List.rev $4) }

simple_stmt:
    | empty_stmt { LinedStatement($startpos.pos_lnum, $1) }
    | expression_stmt { LinedStatement($startpos.pos_lnum, $1) }
    | assign_stmt { LinedStatement($startpos.pos_lnum, $1) }
    | shortvardecl_stmt { LinedStatement($startpos.pos_lnum, $1) }
    | incdec_stmt { LinedStatement($startpos.pos_lnum, $1) }


(* ----- expressions ----- *)

expr:
    | unary_exp { $1 } 
    | binary_exp { $1 }

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
  | unary_op unary_exp %prec uop { UnaryExp($1, $2) }  (* Unary operators have highest precedence *)

primary_expression: 
  | identifier { IdExp $1 } 
  | literal { LiteralExp $1 } 
  | function_call { $1 } 
  | index_exp { $1 } 
  | append_exp { $1 } 
  | select_exp  { $1 }
  | type_cast_exp { $1 }

identifier: 
  | ID { IdName($1) }
  | TBLANKID { BlankID }
unary_op:
  | TPLUS { UPlus } 
  | TMINUS { UMinus } 
  | TNOT { UNot } 
  | TCARET { UCaret } 


function_call:
  | unary_exp TLPAR function_arguments TRPAR
      { FunctionCallExp($1, $3)}

function_arguments: 
  | (* empty *) { [] }
  | non_empty_function_arguments { List.rev $1 }

non_empty_function_arguments:
  | expr { [ $1 ] }
  | non_empty_function_arguments TCOM expr { $3::$1 }
 
index_exp: 
  | unary_exp TLBR expr TRBR 
      { IndexExp($1, $3)} 

append_exp:
  | APPEND TLPAR ID TCOM expr TRPAR   { AppendExp(IdName($3), $5) }

select_exp: 
  | unary_exp TDOT ID 
      { SelectExp($1, IdName($3)) }

type_cast_exp:
  | castable_type TLPAR expr TRPAR 
      { TypeCastExp($1, $3) }

castable_type: 
  | INT_TYP { BasicType(IntType) }
  | FL_TYP { BasicType(FloatType) } 
  | RUNE_TYP { BasicType(RuneType) }
  | BOOL_TYP { BasicType(StringType) } 
 
binary_exp:
  | e1 = expr; op = binary_op; e2 = expr { BinaryExp(op, e1, e2) }

%inline binary_op:
  | TOR { BinOr }
  | TAND {BinAnd }
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
  | TANDAS { SinAnd } 
  | TORAS  { SinOr } 
  | TXORAS { SinXor } 
  | TLAS   { SinLas } 
  | TRAS   { SinRas }
  | TANEQ  { SinAneq }

%%
