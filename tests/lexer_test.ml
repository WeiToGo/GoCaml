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

let rec batch_check_lexer test_string_list output_list = 
  (* Both the argument lists should of course be of the same size.
     I wish ocaml had dictionaries like Python *)
  match test_string_list, output_list with 
  | h::t, h'::t' -> check_lexer h h'; batch_check_lexer t t'
  | [], [] -> ()
  | _ -> failwith "Fatal: Mismatched list length in test case"

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
  batch_check_lexer code_snippets expected_outputs

let string_test test_ctxt = 
  let code_snippets = 
    ["\"this is a \t string\"";
       "`a raw  +- \\n string`";
       "'rune \n newline'"

    ] in 
  let expected_outputs = [
    [TSTR("this is a \t string")];
    [TRWSTR("a raw  +- \\n string")];
    [TRUNE("rune \n newline")]
  ]
  in
  batch_check_lexer code_snippets expected_outputs


let suite = 
"suite">:::
  [
    "Insert semicolon after break!">:: test1;
     "Hello is an id">:: hello_test;
     "Lots of semicolon test">:: lots_of_semicolon_test;
     "String test">:: string_test
   ]

let () = 
  run_test_tt_main suite