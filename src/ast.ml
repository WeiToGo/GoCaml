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

type program = Program of package_decl * (top_decl list)
and package_decl = Package of string
and top_decl = FunctionDecl of function_decl 
      | TypeDeclBlock of (type_declaration list)
      | VarDeclBlock of (multiple_var_declaration list)
(* This is a var decl block: 
  var(
      a,b int = 1,2      <----- MultipleVarDecl           
      c string = "foo"   <----- MultipleVarDecl
    ) 

  In total, [MultipleVarDecl(~~first one~~), MultipleVarDecl(~~second one~~)] 

  Looking at the first MultipleVarDecl:
    a,b int = 1,2
==  [SingleVarDecl a int 1, SingleVarDecl b int 2]
*)
and multiple_var_declaration = MultipleVarDecl of (single_var_declaration list)
and single_var_declaration = SingleVarDecl of (identifier * (type_spec option) * (expression option))
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
and identifier = IdName of string | BlankID
and function_decl = Function of (identifier * function_signature * (statement list))
and function_signature = FunctionSig of ((function_arg list) * (type_spec option))
and function_arg = FunctionArg of (identifier * type_spec)
and expression =
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
    | VarDeclBlockStatement of (multiple_var_declaration list) 
    | PrintStatement of (expression list)
    | PrintlnStatement of (expression list)
    | IfStatement of ((statement option) * expression * (statement list) * ((statement list ) option))
    | ReturnStatement of (expression option)
    | SwitchStatement of ((statement option) * expression * (switch_case list))
    | ForStatement of ((statement option) * expression * (statement option) * (statement list)) 
          (* ForStamenet(init statement, condition (*true by default*), post statement, loop body *)
    | BreakStatement
    | ContinueStatement 
    | BlockStatement of (statement list)
and switch_case = SwitchCase of ((expression list)* (statement list)) | DefaultCase of (statement list)
