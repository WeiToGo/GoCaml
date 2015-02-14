open OUnit2
open Parser

let tokenizer lexer lexbuf =
  let rec aux lexer lexbuf acc =  
    match lexer lexbuf with
    | TEOF -> acc
    | tk -> aux lexer lexbuf (tk::acc)
  in List.rev (aux lexer lexbuf [])

let test1 test_ctxt = 
  let test_string = "break\n" in 
  let lexbuf = Lexing.from_string test_string in
  let lexed_tokens = tokenizer Scan.wrapped_scan lexbuf in
  assert_equal
    ~msg: "Token value"
    ~printer: Printer.string_of_token_list
    [BREAK; TSEMCOL]
    lexed_tokens

let hello_test test_ctxt = 
  let test_string = "hello" in 
  let lexbuf = Lexing.from_string test_string in
  let lexed_tokens = tokenizer Scan.wrapped_scan lexbuf in
  assert_equal
    ~msg: "Hello should be an identifier"
    ~printer: Printer.string_of_token_list
    [ID("hello")]
    lexed_tokens

let check_lexer test_string tklist = 
  let lexbuf = Lexing.from_string test_string in 
  let lexed_tokens = tokenizer Scan.wrapped_scan lexbuf in 
  assert_equal 
    ~msg: "Semicolon insertion not working properly"
    ~printer: Printer.string_of_token_list
    tklist
    lexed_tokens

let lots_of_semicolon_test test_ctxt = 
  let code_snippets = 
    [  "()\n\n\n";
       "{\n}";
       "x++\n"

    ] in 
  let expected_outputs = [
    [TLPAR;TRPAR;TSEMCOL;];
    [TLCUR; TRCUR];
    [ID("x"); TINC; TSEMCOL]
  ]
  in
  let rec aux codes outputs = 
    match codes, outputs with 
    | h::t, h'::t' -> check_lexer h h'; aux t t'
    | [], [] -> ()
    | _ -> failwith "Fatal: Mismatched list length in test case"
  in
  aux code_snippets expected_outputs

let string_test test_ctxt = 
  let code_snippets = 
    [  "\"this is a
        string \"";
       "`a raw  +-
        string`";
       "' rune
       newline'"

    ] in 
  let expected_outputs = [
    [TSTR("this is a \n string")];
    [TRWSTR("a raw \\t string")];
    [TRUNE("rune \n newline")]
  ]
  in
  let rec aux1 codes outputs = 
    match codes, outputs with 
    | h::t, h'::t' -> check_lexer h h'; aux t t'
    | [], [] -> ()
    | _ -> failwith "Fatal: Mismatched list length in test case"
  in
  aux1 code_snippets expected_outputs


let suite = 
"suite">:::
  ["Insert semicolon after break!">:: test1;
   "Hello is an id">:: hello_test;
   "Lots of semicolon test">:: lots_of_semicolon_test]

let () = 
  run_test_tt_main suite