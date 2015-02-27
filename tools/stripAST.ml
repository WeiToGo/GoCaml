(*Strips ast info in between braces{} run with ocaml stripAST.ml file  *)

open Printf

let input = open_in Sys.argv.(1)

let charStream = Stream.from (fun _ -> try Some(input_char input) with End_of_file -> None )

let rec printBody ch =
   print_char ch;
   flush stdout;
   match ch with
   | '{' -> skipChars (Stream.next charStream)
   | '(' -> (match Stream.peek charStream with
            | Some('*') -> skipComment (Stream.next charStream)
            | _         -> printBody (Stream.next charStream))
   | _ -> printBody (Stream.next charStream)
and skipChars ch =
   match ch with
   | '}' -> print_char ch; printBody (Stream.next charStream)
   | '(' -> (match Stream.peek charStream with
            | Some('*') -> skipComment (Stream.next charStream)
            | _         -> skipChars (Stream.next charStream))
   | _ -> skipChars (Stream.next charStream)
and skipComment ch =
   match ch with
   | '*' -> print_char ch;
            flush stdout;
            (match Stream.peek charStream with
            | Some(')') -> printBody (Stream.next charStream)
            | _         -> skipComment (Stream.next charStream))
   | _   -> print_char ch;
            flush stdout;
            skipComment (Stream.next charStream)

let rec printAll ch =
   print_char ch;
   flush stdout;
   match ch with
   | '%' -> (match Stream.peek charStream with
            | Some('%') -> printBody (Stream.next charStream)
            | None -> close_in input
            | _   -> printAll (Stream.next charStream))
   | _   -> printAll (Stream.next charStream)

let () =
   try
      printAll (Stream.next charStream)
   with e ->
      close_in input
