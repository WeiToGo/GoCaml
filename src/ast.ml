type int_literal = DecInt of string | HexInt of string | OctalInt of string
and float_literal = FloatLit of string
and rune_literal = RuneLit of string
and string_literal = StringLit of string


type program = Program of package_decl * (top_decl list)
and package_decl = Package of string
and top_decl = FunctionDecl of function_decl 
      | TypeDecl of (type_declaration list) 
      | VarDecl of (var_declaration list)
and type_declaration = SingleTypeDecl of (identifier * type_spec)
and type_spec = 
      | BasicType of basic_type
      | SliceType of type_spec
      | ArrayType of (int_literal * type_spec)
      | CustomType of identifier
and basic_type = IntType | FloatType | BoolType | RuneType | StringType
and identifier = IdName of string
and function_decl = Function of string (* Not implemented *)
and var_declaration = VarDecl of string (* Not implemented *)