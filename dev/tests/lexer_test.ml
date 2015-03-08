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

let check_lexer test_string tklist ~msg = 
  let lexbuf = Lexing.from_string test_string in 
  let lexed_tokens = tokenizer Scan.wrapped_scan lexbuf in 
  assert_equal
    ~msg: msg
    ~printer: Printer.string_of_token_list
    tklist
    lexed_tokens

let rec batch_check_lexer test_string_list output_list ~msg = 
  (* Both the argument lists should of course be of the same size.
     I wish ocaml had dictionaries like Python *)
  match test_string_list, output_list with 
  | h::t, h'::t' -> check_lexer h h' ~msg: msg; batch_check_lexer t t' ~msg: msg
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
  batch_check_lexer code_snippets expected_outputs ~msg: "Semicolon insertion not working properly"

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
  batch_check_lexer code_snippets expected_outputs ~msg: "Strings not working properly"




let suite = 
"suite">:::
  [
    "Insert semicolon after break!">:: test1;
    "Hello is an id">:: hello_test;
    "Lots of semicolon test">:: lots_of_semicolon_test;
    "String test">:: string_test;

    (
      "dec_int_test">::
        fun test_ctxt -> batch_check_lexer
          [ "45"; "123"; "0"] [ [DEC_INT("45")]; [DEC_INT("123")]; [DEC_INT("0")]]
          ~msg: "Decimal int lexing failed"
    );
    (
      "octal_int_test">::
        fun test_ctxt -> batch_check_lexer 
          [ "0234"] [ [OCTAL_INT("0234")]]
          ~msg: "Octal int lexing failed"
    );
    (
      "hex_int_test">::
        fun test_ctxt -> batch_check_lexer
          [ "0xabcdef"; "0x0"] [ [HEX_INT("0xabcdef")]; [HEX_INT("0x0")]]
          ~msg: "Hex int lexing failed"
    );
   ]

let () = 
  run_test_tt_main suite