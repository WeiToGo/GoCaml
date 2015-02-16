
{
  open Parser

  exception Error of string

  (* Hah sneaking in a reference variable here. Not so pure are we. *)
  let last_token: (token option ref) = ref None

}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let letter = ['A'-'Z' 'a'-'z' '_']
let digit = ['0'-'9']
let decimal_digit = ['0'-'9'] 
let nz_digit = ['1'-'9']  (* Non-zero digit *) 
let octal_digit = ['0' - '7']
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']


(* operators + delimeters *)
rule scan last_token = parse

  (* Whitespace and comments *)
  | white         { scan last_token lexbuf }
  | newline       { match last_token with
                    | Some(BREAK)
                    | Some(ID _)
                    (* Add cases for int, float imaginary, rune and string here *)
                    | Some(TINC) | Some(TDECR)
                    | Some(TRBR) | Some(TRPAR) | Some(TRCUR)
                      -> TSEMCOL
                    | _ -> scan None lexbuf 
                  } 

  (* Keywords *)
  | "break"       { BREAK }
  | "case"        { CASE }
  | "chan"        { CHAN }
  | "const"       { CONST }
  | "continue"    { CONT }
  | "default"     { DEFAULT }
  | "defer"       { DEFER }
  | "else"        { ELSE }
  | "fallthrough" { FALLTHROUGH }
  | "for"         { FOR }
  | "func"        { FUNC }
  | "go"          { GO }
  | "goto"        { GOTO }
  | "if"          { IF }
  | "import"      { IMPORT }
  | "interface"   { INTERFACE }
  | "map"         { MAP }
  | "package"     { PACKAGE }
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
  | letter (letter|digit)* as id_string  { ID(id_string) }

  (* Symbols and operators *)
  | '+'   { TPLUS }
  | '-'   { TMINUS }
  | '*'   { TMULT }
  | '/'   { TDIV }
  | '%'   { TMOD }
  | '&'   { TBITAND }
  | '|'   { TBITOR }
  | '^'   { TCARET }
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
  | "--"  { TDECR }
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
  | '_'   { TBLANKID }

  (* Literals *)
  | '`'   { read_raw_str (Buffer.create 15) lexbuf } (* raw string token *) 
  | '"'   { read_string (Buffer.create 15) lexbuf } (* interpreted string token *) 
  | '''   { read_rune (Buffer.create 2) lexbuf } (* raw string token *) 
  | nz_digit decimal_digit* as st { DEC_INT(st)}
  | "0" octal_digit* as st { OCTAL_INT(st) }
  | "0" ("x" | "X" ) hex_digit* as st { HEX_INT(st) }
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
  | [^''' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_rune buf lexbuf}
  | _ as c { raise (Error (Printf.sprintf "Scanner: Illegal character: %c\n" c))}

  
and read_raw_str buf = parse
  | '`' { TRWSTR (Buffer.contents buf)}
  | [^'`' '\r']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_raw_str buf lexbuf}

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
  | [^'"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}
  | _ as c { raise (Error (Printf.sprintf "Scanner: Illegal character: %c\n" c))}

{
  (* wrapped_scan keeps track of the last token returned by scan. *)
  let wrapped_scan lexbuf = 
    let new_token = scan (!last_token) lexbuf in 
    last_token := Some(new_token); new_token
}