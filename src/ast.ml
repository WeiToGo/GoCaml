type program = Program of package_decl * (top_decl list)
and package_decl = Package of string
and top_decl = FunctionDecl of function_decl 
      | TypeDeclBlock of (type_declaration list)
      | VarDeclBlock of (var_declaration list)
and var_declaration = 
      | MultipleVarDecl of ((identifier * (type_spec option) * (expression option)) list)
and type_declaration = 
      | SingleTypeDecl of (identifier * type_spec)
and type_spec = 
      | BasicType of basic_type
      | SliceType of type_spec
      | ArrayType of (int_literal * type_spec)
      | StructType of struct_definition
      | CustomType of identifier
and struct_definition = StructDefinition of (struct_field_list list)
and struct_field_list = StructFieldList of (struct_field list)
and struct_field = StructField of (identifier * type_spec) 
and basic_type = IntType | FloatType | BoolType | RuneType | StringType
and identifier = IdName of string
and function_decl = Function of ((function_arg list) * (type_spec option) * (statement list))
and function_arg = FunctionArg of (identifier * type_spec)
and expression =
    | IdExp of identifier 
    | LiteralExp of literal
    | UnaryExp of (unary_op * expression) 
    | BinaryExp of (binary_op * expression * expression)
    | FunctionCallExp of (identifier * (expression list))
    | AppendExp of (identifier * expression)
    | TypeCastExp of (type_spec * expression)
    | IndexExp of (expression * int)
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
    | BinEq | BinNotEq | BinLess | BinLessEq | BinGreater | BinGreaterEqual
    | BinPlus | BinMinus | BinBitOr | BinBitXor
    | BinMult | BinDiv | BinMod | BinShiftLeft | BinShiftRight | BinBitAnd | BinBitAndNot
and statement = 
    | EmptyStatement
    | ExpressionStatement of expression
    | AssignmentStatement of ((lvalue * expression) list) 
    | TypeDeclStatement of (type_declaration list)
    | VarDeclStatement of var_declaration
    | PrintStatemet of (expression list)
    | PrintlnStamement of (expression list)
    | IfStatement of ((statement option) * expression * (statement list) * ((statement list ) option))
    | ReturnStatement of (expression option)
    | SwitchStatement of ((statement option) * expression * (switch_case list))
    | ForStament of (statement * expression * statement * (statement list)) (* Weed out short_var_declr in post-statement *)
    | BreakStatement
    | ContinueStatement
and switch_case = SwitchCase of (expression * (statement list))
and lvalue = 
    | LId of identifier
    | LIndex of (lvalue * int)
    | LSelector of (lvalue * identifier)
