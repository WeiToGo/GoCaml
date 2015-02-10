
{
  open Token

  exception Error of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* operators + delimeters *)
rule scan = parse
  | white { scan lexbuf }
  | '+' { TPLUS }
  | '-' { TMINUS }
  | '*' { TMULT }
  | '/' { TDIV }
  | '%' { TMOD }
  | '&' { TBITAND }
  | '|' { TBITOR }
  | '^' { TBITXOR }
  | "<<" { TLSFT }
  | ">>" { TRSFT }
  | "&^" { TANOT }
  | "+=" { TADDAS }
  | "-=" { TSUBAS }
  | "*=" { TMULAS }
  | "/=" { TDIVAS }
  | "%=" { TMODAS }
  | "&=" { TANDAS }
  | "|=" { TORAS }
  | "^=" { TXORAS }
  | "<<=" { TLAS }
  | ">>=" { TRAS }
  | "&^=" { TANEQ}
  | "&&" { TAND }
  | "||" { TOR }
  | "<-" { TREC }
  | "++" { TINC }
  | "==" { TEQ }
  | '<' { TLS }
  | '>' { TGR }
  | '=' { TASSIGN }
  | '!' { TNOT}
  | "!=" { TNEQ }
  | "<=" { TLSEQ }
  | ">=" { TGREQ}  
  | ":=" { TCOLEQ }
  | "..." { TTD }
  | '(' { TLPAR }
  | ')' { TRPAR }
  | '[' { TLBR }
  | ']' { TRBR }
  | '{' { TLCUR }
  | '}' { TRCUR }
  | ',' { TCOM }
  | '.' { TDOT }
  | ';' { TSEMCOL}
  | ':' { TCOL }
  | '"'   { read_string (Buffer.create 15) lexbuf } (* interp string token *)
  | "[^]*"    { TRWSTR }
  | ''' { TRUNE }
  | [' ' '\t' '\n' '\r']	{ scan lexbuf }	(* ignore whitespace and newlines *)
  | eof   { EOF}

and scan_string buf = parse
  | '"' { STR (Buffer.contents buf) }
  | '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf}
  | '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf}
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf}
  | '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf}
  | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf}
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf}
  | '\\' ''' { Buffer.add_char buf '\''; read_string buf lexbuf}
  | '\\' '"' { Buffer.add_char buf '\"'; read_string buf lexbuf}
  | [^'"' '\\']+ { Buffer.add_char buf (Lexing.lexeme lexbuf);
                    read_string buf lexbuf }
  | _ as c { raise (Error (Printf.sprintf "Scanner: Unrecognized character: %c\n" c))
  }