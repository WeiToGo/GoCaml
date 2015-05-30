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

type assignment_ops = | SinADD
                      | SinSUB
                      | SinMul
                      | SinDiv
                      | SinMod
                      | SinAnd
                      | SinOr
                      | SinXor
                      | SinLas
                      | SinRas
                      | SinAneq

type program = Program of package_decl * (lined_top_decl list)
and package_decl = Package of (string * int)  (* Package of package_name * line_number *)
and lined_top_decl = LinedTD of (top_decl * int) 
and top_decl = FunctionDecl of (identifier * function_signature * (statement list) )
      | TypeDeclBlock of (type_declaration list)
      | VarDeclBlock of (multiple_var_declaration list)
(* This is a var decl block: 
 *  var(
 *      a,b int = 1,2      <----- MultipleVarDecl           
 *      c string = "foo"   <----- MultipleVarDecl
 *    ) 
 *
 *  In total, [MultipleVarDecl(~~first one~~), MultipleVarDecl(~~second one~~)] 
 *
 *  Looking at the first MultipleVarDecl:
 *    a,b int = 1,2
 * => [SingleVarDecl a int 1, SingleVarDecl b int 2]
  *)
and multiple_var_declaration = MultipleVarDecl of (single_var_declaration list)
and single_var_declaration = SingleVarDecl of (identifier * (type_spec option) * (expression option))
and short_var_decl = ShortVarDecl of (identifier * expression)
and type_declaration = 
      | SingleTypeDecl of (identifier * type_spec)
and type_spec = 
      | BasicType of basic_type
      | SliceType of type_spec
      | ArrayType of (int_literal * type_spec)
      | StructType of (multi_struct_field_decl list)
      | FunctionType of (type_spec list * type_spec option)  (* function argument type list x function return type *)
      | CustomType of identifier
and multi_struct_field_decl = MultipleStructFieldDecl of (single_struct_field_decl list)
and single_struct_field_decl = SingleStructFieldDecl of (identifier * type_spec) 
and basic_type = IntType | FloatType | BoolType | RuneType | StringType
and identifier = ID of (string * Symtable.sym_table_entry option ref) | BlankID
and function_signature = FunctionSig of ((function_arg list) * (type_spec option))
and function_arg = FunctionArg of (identifier * type_spec)
and expression = Expression of (exp * Symtable.gotype option ref)
and exp =
    | IdExp of identifier 
    | LiteralExp of literal
    | UnaryExp of (unary_op * expression) 
    | BinaryExp of (binary_op * expression * expression)
    | FunctionCallExp of (expression * (expression list))
    | AppendExp of (identifier * expression)
    | TypeCastExp of (type_spec * expression)
    | IndexExp of (expression * expression)
    | SelectExp of (expression * identifier) 
and literal = 
    | IntLit of int_literal
    | FloatLit of string
    | RuneLit of string
    | StringLit of string
    | RawStringLit of string
and int_literal = DecInt of string | HexInt of string | OctalInt of string
and unary_op = UPlus | UMinus | UNot | UCaret
and binary_op = 
    | BinOr | BinAnd 
    | BinEq | BinNotEq | BinLess | BinLessEq | BinGreater | BinGreaterEq
    | BinPlus | BinMinus | BinBitOr | BinBitXor
    | BinMult | BinDiv | BinMod | BinShiftLeft | BinShiftRight | BinBitAnd | BinBitAndNot
and statement = LinedStatement of int * plain_statement    
and plain_statement = 
    | EmptyStatement
    | ExpressionStatement of expression
    | AssignmentStatement of ((expression * expression) list) 
    | TypeDeclBlockStatement of (type_declaration list)
    | ShortVarDeclStatement of (short_var_decl list)
    | VarDeclBlockStatement of (multiple_var_declaration list) 
    | PrintStatement of (expression list)
    | PrintlnStatement of (expression list)
    | IfStatement of ((statement option) * expression * (statement list) * ((statement list ) option))
    | ReturnStatement of (expression option)
    | SwitchStatement of ((statement option) * expression * (switch_case list))
    | ForStatement of ((statement option) * expression option * (statement option) * (statement list)) 
          (* ForStamenet(init statement, condition (*true by default*), post statement, loop body *)
    | BreakStatement
    | ContinueStatement 
    | BlockStatement of (statement list)
and switch_case = 
    | SwitchCase of ((expression list)* (statement list)) 
    | DefaultCase of (statement list)
