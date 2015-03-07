
{
  open Parser

  exception Error of string
  exception NotSupportedInGoLite

  (* Hah sneaking in a reference variable here. Not so pure are we. *)
  let last_token: (token option ref) = ref None

  let needs_semicolon last_token = match last_token with
  | Some(BREAK)
  | Some(TID _)
  | Some(TBLANKID)
  | Some(TINC) | Some(TDECR)
  | Some(RETURN) | Some(CONT)
  | Some(DEC_INT(_)) | Some(HEX_INT(_)) | Some(OCTAL_INT(_))
  | Some(FLOAT64(_))
  | Some(TRUNE(_)) | Some(TRWSTR(_)) | Some(TSTR(_))
  | Some(TRBR) | Some(TRPAR) | Some(TRCUR)
  | Some(INT_TYP) | Some(FL_TYP) | Some(BOOL_TYP) | Some(RUNE_TYP) | Some(STR_TYP)
    -> true
  | _ -> false 

}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let letter = ['A'-'Z' 'a'-'z']
let uscore = '_'
let digit = ['0'-'9']
let decimal_digit = ['0'-'9'] 
let nz_digit = ['1'-'9']  (* Non-zero digit *) 
let octal_digit = ['0' - '7']
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let character = ['A'-'Z' 'a'-'z' '_' '0'-'9' ' ' '\t']
let a = [^ '*' '/']


(* operators + delimeters *)
rule scan last_token = parse

  (* Whitespace and comments *)  
  | white         { scan last_token lexbuf }

  | newline       { Lexing.new_line lexbuf;
                    if (needs_semicolon last_token) then TSEMCOL
                    else scan None lexbuf
                  } 
  | "//" [^ '\n']* { scan last_token lexbuf }
  | "/*" { read_comment last_token lexbuf }

  (* Keywords *)
  | "break"       { BREAK }
  | "case"        { CASE }
  | "chan"        { raise NotSupportedInGoLite }
  | "const"       { raise NotSupportedInGoLite }
  | "continue"    { CONT }
  | "default"     { DEFAULT }
  | "defer"       { raise NotSupportedInGoLite }
  | "else"        { ELSE }
  | "fallthrough" { raise NotSupportedInGoLite }
  | "for"         { FOR }
  | "func"        { FUNC }
  | "go"          { raise NotSupportedInGoLite }
  | "goto"        { raise NotSupportedInGoLite }
  | "if"          { IF }
  | "import"      { raise NotSupportedInGoLite }
  | "interface"   { raise NotSupportedInGoLite }
  | "map"         { raise NotSupportedInGoLite }
  | "package"     { PACKAGE }
  | "range"       { raise NotSupportedInGoLite }
  | "return"      { RETURN }
  | "select"      { raise NotSupportedInGoLite }
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
  | letter (letter|digit|uscore)* | uscore (letter|digit|uscore)+ as id_string  { TID(id_string) }

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
  | "<-"  { raise NotSupportedInGoLite }
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
  | "..." { raise NotSupportedInGoLite }
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
  | (nz_digit decimal_digit* | "0") as st { DEC_INT(st)}
  | "0" octal_digit+ as st { OCTAL_INT(st) }
  | "0" ("x" | "X" ) hex_digit+ as st { HEX_INT(st) }
  | decimal_digit+ "." decimal_digit* as st { FLOAT64(st)}
  | "." decimal_digit+ as st { FLOAT64(st) }
  | eof   { TEOF }
  | _ as c { raise (Error (Printf.sprintf "Scanner: Illegal character: %c\n" c))}

and read_rune buf = parse
  | '\\' 'a' { Buffer.add_char buf '\007'; read_single_quote buf lexbuf}
  | '\\' 'b' { Buffer.add_char buf '\010'; read_single_quote buf lexbuf}
  | '\\' 'f' { Buffer.add_char buf '\014'; read_single_quote buf lexbuf}
  | '\\' 'n' { Buffer.add_char buf '\012'; read_single_quote buf lexbuf}
  | '\\' 'r' { Buffer.add_char buf '\015'; read_single_quote buf lexbuf}
  | '\\' 't' { Buffer.add_char buf '\011'; read_single_quote buf lexbuf}
  | '\\' 'v' { Buffer.add_char buf '\013'; read_single_quote buf lexbuf}
  | '\\' '\\' { Buffer.add_char buf '\\'; read_single_quote buf lexbuf}
  | '\\' ''' { Buffer.add_char buf '\''; read_single_quote buf lexbuf}
  | [^''' '\\' '\n'] { Buffer.add_string buf (Lexing.lexeme lexbuf); read_single_quote buf lexbuf}

and read_single_quote buf = parse
  | ''' { TRUNE (Buffer.contents buf) }
and read_raw_str buf = parse
  | '`' { TRWSTR (Buffer.contents buf)}
  | [^'`' '\r']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_raw_str buf lexbuf}

and read_string buf = parse
  | '"' { TSTR (Buffer.contents buf)}
  | '\\' 'a' { Buffer.add_char buf '\007'; read_string buf lexbuf}
  | '\\' 'b' { Buffer.add_char buf '\010'; read_string buf lexbuf}
  | '\\' 'f' { Buffer.add_char buf '\014'; read_string buf lexbuf}
  | '\\' 'n' { Buffer.add_char buf '\012'; read_string buf lexbuf}
  | '\\' 'r' { Buffer.add_char buf '\015'; read_string buf lexbuf}
  | '\\' 't' { Buffer.add_char buf '\011'; read_string buf lexbuf}
  | '\\' 'v' { Buffer.add_char buf '\013'; read_string buf lexbuf}
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf}
  | '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf}
  | [^'"' '\\' '\n']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}

and read_comment last_token = parse
  | "*/" { scan last_token lexbuf }
  | '\n'{ Lexing.new_line lexbuf; read_comment last_token lexbuf }
  | _ { read_comment last_token lexbuf}

{
  (* wrapped_scan keeps track of the last token returned by scan. *)
  let wrapped_scan lexbuf = 
    let new_token = scan (!last_token) lexbuf in 
    last_token := Some(new_token); new_token
}
