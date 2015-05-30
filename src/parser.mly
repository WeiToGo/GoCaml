(*

  This file is part of GoCaml, a compiler that compiles Go-Lite (a subset of Go) to Java bytecode. 

  Copyright (C) 2015 by Deepanjan Roy, Wei Gao, Omar Gonzalez 


  GoCaml is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  GoCaml is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Foobar.  If not, see <http://www.gnu.org/licenses/>. 

  This code originated as a project for the COMP 520 class at McGill University
  in Winter 2015. Any subsequent COMP 520 student who is viewing this code must 
  follow the course rules and report any viewing and/or use of the code.

*)

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
%token<string> TID
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
  | package_decl TSEMCOL lined_top_decl_list TEOF  { Program($1, List.rev $3) }

package_decl:
  | PACKAGE TID { Package($2, $startpos.pos_lnum) }


lined_top_decl_list :
  | (* empty *)   { [] }
  | lined_top_decl_list lined_top_decl TSEMCOL { $2 :: $1 } 

lined_top_decl:
  | top_decl { LinedTD($1, $startpos.pos_lnum)}

top_decl :
  | declaration { $1 } 
  | func_decl { $1 } 

(*-----------*)

declaration :
  | var_decl { VarDeclBlock $1 }
  | typ_decl { TypeDeclBlock $1 }

func_decl: 
  | FUNC TID func_signature TLCUR stmt_list TRCUR 
      { FunctionDecl(ID($2, ref None), $3, List.rev $5) } 

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
  | id_list TLPAR typ TRPAR
    { MultipleVarDecl(distribute $1 $3 [] (fun x y -> SingleVarDecl(x, Some y, None))) }    
  | id_list TASSIGN expr_list
    {  MultipleVarDecl(list_zip $1 $3 [] (fun x y -> SingleVarDecl(x, None, Some y)))  }
  | id_list typ TASSIGN expr_list 
    { MultipleVarDecl(list_zip $1 $4 [] (fun x y -> SingleVarDecl(x, Some $2, Some y))) }

var_spec_list:
  | (* empty *) { [] }
  | var_spec_list var_spec TSEMCOL { $2 :: $1 } 

typ_spec:
  | identifier typ  { SingleTypeDecl($1, $2) }

typ_spec_list:
  | (* empty *) { [] }
  | typ_spec_list typ_spec TSEMCOL { $2 :: $1 }

typ:
  | basic_typ  { BasicType $1 }
  | slice_typ { $1 }
  | array_typ { $1 }
  | struct_typ { $1 }
  | func_typ { $1 } 
  | identifier { CustomType($1) }

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
    | expr { ExpressionStatement(Expression($1, ref None)) }

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
      AssignmentStatement([(Expression(lv, ref None), Expression(BinaryExp(binop_of_assignment_op op, Expression(lv, ref None), Expression(exp, ref None)), ref None))])     
     }
  | expr_list TASSIGN expr_list
     { AssignmentStatement(list_zip $1 $3 []  (fun x y -> (x,y)) ) }

expr_list:
    | expr_list TCOM expr { Expression($3, ref None) :: $1 }
    | expr { [ Expression($1, ref None) ] }

declaration_stmt:
    | var_decl 
        { VarDeclBlockStatement $1 }
    | typ_decl
        { TypeDeclBlockStatement $1 }

shortvardecl_stmt:  
    | expr_list TCOLEQ expr_list
      {
       let id_of_expr xp = match xp with
        | Expression(IdExp(id), _) -> id
        | _ -> raise NonIDExpr 
        in
        let id_list = List.map id_of_expr $1 in
        ShortVarDeclStatement(
            list_zip id_list $3 [] (fun x y -> ShortVarDecl(x, y) )
        )
      }
 
incdec_stmt:
    | primary_expression TINC 
        { AssignmentStatement([(Expression($1, ref None), 
			Expression(
				BinaryExp(BinPlus, Expression($1, ref None), 
        			Expression(LiteralExp(IntLit(DecInt("1"))), ref None)
      			),
				ref None
			)
		)])
        }
    | primary_expression TDECR
        { AssignmentStatement([(Expression($1, ref None),
			Expression(
				BinaryExp(
					BinMinus, Expression($1, ref None),
        			Expression(LiteralExp(IntLit(DecInt("1"))), ref None)
				),
				ref None
			)
		)])    
        }


print_stmt:
    | PRINT TLPAR TRPAR { PrintStatement([]) }
    | PRINT TLPAR expr_list TRPAR { PrintStatement(List.rev $3) }

println_stmt:
    | PRINTLN TLPAR TRPAR { PrintlnStatement([]) }
    | PRINTLN TLPAR expr_list TRPAR { PrintlnStatement(List.rev $3) }

return_stmt:
    | RETURN { ReturnStatement(None) }
    | RETURN expr { ReturnStatement(Some(Expression($2, ref None))) }




if_stmt:
    | IF expr TLCUR stmt_list TRCUR 
        { IfStatement(None, Expression($2, ref None), List.rev $4, None) }
    | IF expr TLCUR stmt_list TRCUR ELSE TLCUR stmt_list TRCUR 
        { IfStatement(None, Expression($2, ref None), List.rev $4, Some (List.rev $8)) }
    | IF expr TLCUR stmt_list TRCUR ELSE if_stmt 
        { IfStatement(None, Expression($2, ref None), List.rev $4,
           Some [LinedStatement($startpos.pos_lnum, $7)]) }
    | IF simple_stmt TSEMCOL expr TLCUR stmt_list TRCUR 
        { IfStatement(Some $2, Expression($4, ref None), List.rev $6, None) }
    | IF simple_stmt TSEMCOL expr TLCUR stmt_list TRCUR ELSE TLCUR stmt_list TRCUR 
        { IfStatement(Some $2, Expression($4, ref None), List.rev $6, Some (List.rev $10)) }
    | IF simple_stmt TSEMCOL expr TLCUR stmt_list TRCUR ELSE if_stmt 
        { IfStatement(Some $2, Expression($4, ref None), List.rev $6,
           Some [LinedStatement($startpos.pos_lnum, $9)]) }

switch_stmt:
    | SWITCH TLCUR switch_clause_list TRCUR 
        { SwitchStatement(None, Expression(IdExp(ID("true", ref None)), ref None), List.rev $3) }
    | SWITCH expr TLCUR switch_clause_list TRCUR 
        { SwitchStatement(None, Expression($2, ref None), List.rev $4) }
    | SWITCH simple_stmt TSEMCOL TLCUR switch_clause_list TRCUR 
        { SwitchStatement(Some $2, Expression(IdExp(ID("true", ref None)), ref None), List.rev $5)}
    | SWITCH simple_stmt TSEMCOL expr TLCUR switch_clause_list TRCUR 
        { SwitchStatement(Some $2, Expression($4, ref None), List.rev $6) } 

for_stmt:
    | FOR TLCUR stmt_list TRCUR { ForStatement(None, None, None, List.rev $3) }
    | FOR expr TLCUR stmt_list TRCUR { ForStatement(None, Some(Expression($2, ref None)), None, List.rev $4) }
    | FOR simple_stmt TSEMCOL expr TSEMCOL simple_stmt TLCUR stmt_list TRCUR 
        { ForStatement(Some $2, Some(Expression($4, ref None)), Some $6, List.rev $8) } 
    | FOR simple_stmt TSEMCOL TSEMCOL simple_stmt TLCUR stmt_list TRCUR 
        { ForStatement(Some $2, None, Some $5, List.rev $7) }

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
    | raw_string_literal { RawStringLit($1)}

int_literal: 
    | DEC_INT { DecInt $1  }
    | OCTAL_INT { OctalInt $1 }
    | HEX_INT { HexInt $1 }

float_literal: FLOAT64 { $1 }

rune_literal: TRUNE { $1 }

string_literal: TSTR { $1 }

raw_string_literal: TRWSTR { $1 } 

unary_exp:
  | TLPAR expr TRPAR { $2 }
  | primary_expression { $1 }
  | unary_op unary_exp %prec uop { UnaryExp($1, Expression($2, ref None)) }  (* Unary operators have highest precedence *)

primary_expression: 
  | identifier { IdExp $1 } 
  | literal { LiteralExp $1 } 
  | function_call { $1 } 
  | index_exp { $1 } 
  | append_exp { $1 } 
  | select_exp  { $1 }
  | type_cast_exp { $1 }

identifier: 
  | TID { ID($1, ref None) }
  | TBLANKID { BlankID }
unary_op:
  | TPLUS { UPlus } 
  | TMINUS { UMinus } 
  | TNOT { UNot } 
  | TCARET { UCaret } 


function_call:
  | unary_exp TLPAR function_arguments TRPAR
      { FunctionCallExp(Expression($1, ref None), $3)}

function_arguments: 
  | (* empty *) { [] }
  | non_empty_function_arguments { List.rev $1 }

non_empty_function_arguments:
  | expr { [ Expression($1, ref None) ] }
  | non_empty_function_arguments TCOM expr { Expression($3, ref None)::$1 }
 
index_exp: 
  | unary_exp TLBR expr TRBR 
      { IndexExp(Expression($1, ref None), Expression($3, ref None))} 

append_exp:
  | APPEND TLPAR TID TCOM expr TRPAR   { AppendExp(ID($3, ref None), Expression($5, ref None)) }

select_exp: 
  | unary_exp TDOT TID 
      { SelectExp(Expression($1, ref None), ID($3, ref None)) }

type_cast_exp:
  | castable_type TLPAR expr TRPAR 
      { TypeCastExp($1, Expression($3, ref None)) }

castable_type: 
  | INT_TYP { BasicType(IntType) }
  | FL_TYP { BasicType(FloatType) } 
  | RUNE_TYP { BasicType(RuneType) }
  | BOOL_TYP { BasicType(BoolType) } 
 
binary_exp:
  | e1 = expr; op = binary_op; e2 = expr { BinaryExp(op, Expression(e1, ref None), Expression(e2, ref None)) }

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
