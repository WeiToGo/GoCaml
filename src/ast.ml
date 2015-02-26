type program = Program of package_decl * (top_decl list)
and package_decl = Package of string
and top_decl = FunctionDecl of function_decl 
      | TypeDeclBlock of (type_declaration list)
      | VarDeclBlock of (var_declaration list)
and var_declaration = 
      | MultipleVarDecl of (single_var_declaration list)
and single_var_declaration = SingleVarDecl of (identifier * (type_spec option) * (expression option))
and type_declaration = 
      | SingleTypeDecl of (identifier * type_spec)
and type_spec = 
      | BasicType of basic_type
      | SliceType of type_spec
      | ArrayType of (int_literal * type_spec)
      | StructType of (struct_field_decl list)
      | CustomType of identifier
and struct_field_decl = StructFieldDecl of (struct_field list)
and struct_field = StructField of (identifier * type_spec) 
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
    | FunctionCallExp of (identifier * (expression list))
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
    | AssignmentStatement of ((lvalue * expression) list) 
    | TypeDeclBlockStatement of (type_declaration list)
    | VarDeclBlockStatement of (var_declaration list) 
    | PrintStatement of (expression list)
    | PrintlnStatement of (expression list)
    | IfStatement of ((statement option) * expression * (statement list) * ((statement list ) option))
    | ReturnStatement of (expression option)
    | SwitchStatement of ((statement option) * expression * (switch_case list))
    | ForStatement of ((statement option) * expression * (statement option) * (statement list)) (* Weed out short_var_declr in post-statement *)
    | BreakStatement
    | ContinueStatement 
    | BlockStatement of (statement list)
and switch_case = SwitchCase of ((expression list)* (statement list)) | DefaultCase of (statement list)
and lvalue = 
    | LId of identifier
    | LIndex of (lvalue * expression)
    | LSelector of (lvalue * identifier)

let rec exp_of_lvalue lval = match lval with
  | LId(idn) -> IdExp(idn)
  | LIndex(l', x) -> IndexExp(exp_of_lvalue l', x)
  | LSelector(l', idn) -> SelectExp(exp_of_lvalue l', idn) 
