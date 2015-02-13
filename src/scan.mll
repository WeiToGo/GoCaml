
{
  open Token

  exception Error of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"


(* operators + delimeters *)
rule scan = parse
  | white         { scan lexbuf }
  | newline       { scan lexbuf } 
  | "break"       { BREAK }
  | "case"        { CASE }
  | "chan"        { CHAN }
  | "const"       { CONST }
  | "continue"    { CONT }
  | "default"     { DEFAULT }
  | "defer"       { DEF }
  | "else"        { ELSE }
  | "fallthrough" { FT }
  | "for"         { FOR }
  | "func"        { FUNC }
  | "go"          { GO }
  | "goto"        { GOTO }
  | "if"          { IF }
  | "import"      { IMPORT }
  | "interface"   { INTERF }
  | "map"         { MAP }
  | "package"     { PAC }
  | "range"       { RANGE }
  | "return"      { RETURN }
  | "select"      { SELECT }
  | "struct"      { STRUCT }
  | "switch"      { SWITCH }
  | "type"        { TYPE }
  | "var"         { VAR }
  | "int"         { INT_TYP }
  | "float64"     { FL_TYP }
  | "bool"        { BOOL_TYP }
  | "rune"        { RUNE_TYP }
  | "string"      { STR_TYP }
  | "print"       { PRINT }
  | "println"     { PRINTLN }
  | "append"      { APPEND }
  | '+'   { TPLUS }
  | '-'   { TMINUS }
  | '*'   { TMULT }
  | '/'   { TDIV }
  | '%'   { TMOD }
  | '&'   { TBITAND }
  | '|'   { TBITOR }
  | '^'   { TBITXOR }
  | "<<"  { TLSFT }
  | ">>"  { TRSFT }
  | "&^"  { TANOT }
  | "+="  { TADDAS }
  | "-="  { TSUBAS }
  | "*="  { TMULAS }
  | "/="  { TDIVAS }
  | "%="  { TMODAS }
  | "&="  { TANDAS }
  | "|="  { TORAS }
  | "^="  { TXORAS }
  | "<<=" { TLAS }
  | ">>=" { TRAS }
  | "&^=" { TANEQ}
  | "&&"  { TAND }
  | "||"  { TOR }
  | "<-"  { TREC }
  | "++"  { TINC }
  | "=="  { TEQ }
  | '<'   { TLS }
  | '>'   { TGR }
  | '='   { TASSIGN }
  | '!'   { TNOT}
  | "!="  { TNEQ }
  | "<="  { TLSEQ }
  | ">="  { TGREQ}  
  | ":="  { TCOLEQ }
  | "..." { TTD }
  | '('   { TLPAR }
  | ')'   { TRPAR }
  | '['   { TLBR }
  | ']'   { TRBR }
  | '{'   { TLCUR }
  | '}'   { TRCUR }
  | ','   { TCOM }
  | '.'   { TDOT }
  | ';'   { TSEMCOL}
  | ':'   { TCOL }
  | '`'   { read_raw_str (Buffer.create 15) lexbuf } (* raw string token *) 
  | '"'   { read_string (Buffer.create 15) lexbuf } (* interpreted string token *) 
  | '''   { read_rune (Buffer.create 2) lexbuf } (* raw string token *) 
  | eof   { TEOF}


and read_rune buf = parse
  | ''' { TRUNE (Buffer.contents buf)}
  | '\\' 'a' { Buffer.add_char buf '\007'; read_rune buf lexbuf}
  | '\\' 'b' { Buffer.add_char buf '\010'; read_rune buf lexbuf}
  | '\\' 'f' { Buffer.add_char buf '\014'; read_rune buf lexbuf}
  | '\\' 'n' { Buffer.add_char buf '\n'; read_rune buf lexbuf}
  | '\\' 'r' { Buffer.add_char buf '\r'; read_rune buf lexbuf}
  | '\\' 't' { Buffer.add_char buf '\t'; read_rune buf lexbuf}
  | '\\' 'v' { Buffer.add_char buf '\013'; read_rune buf lexbuf}
  | '\\' '\\' { Buffer.add_char buf '\\'; read_rune buf lexbuf}
  | '\\' '\'' { Buffer.add_char buf '\''; read_rune buf lexbuf}
  | '\\' '"' { Buffer.add_char buf '"'; read_rune buf lexbuf}
  | [^''' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf);
                    Buffer.output_buffer stdout buf; read_rune buf lexbuf}
  | _ as c { raise (Error (Printf.sprintf "Scanner: Illegal character: %c\n" c))}

  
and read_raw_str buf = parse
  | '`' { TRWSTR (Buffer.contents buf)}
  | [^'`' '\r']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); Buffer.output_buffer stdout buf; read_raw_str buf lexbuf}

and read_string buf = parse
  | '"' { TSTR (Buffer.contents buf)}
  | '\\' 'a' { Buffer.add_char buf '\007'; read_string buf lexbuf}
  | '\\' 'b' { Buffer.add_char buf '\010'; read_string buf lexbuf}
  | '\\' 'f' { Buffer.add_char buf '\014'; read_string buf lexbuf}
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf}
  | '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf}
  | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf}
  | '\\' 'v' { Buffer.add_char buf '\013'; read_string buf lexbuf}
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf}
  | '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf}
  | [^'"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf);
                    Buffer.output_buffer stdout buf; read_string buf lexbuf}
  | _ as c { raise (Error (Printf.sprintf "Scanner: Illegal character: %c\n" c))}

