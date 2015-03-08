(*Strips ast info in between braces{} run with ocaml stripAST.ml file  *)

open Printf
open String

let input = open_in Sys.argv.(1)

let charStream = Stream.from (fun _ -> try Some(input_char input) with End_of_file -> None )

let outFile =
   let arg = Sys.argv.(1) in
   let pos = index arg '.' in
   let pre = sub arg 0 pos in
   let name = concat "." [pre; "stripped.mly"] in
   open_out name

(* Set of mutually recursive char printing functions.
 * printBody prints everything until you enter brackets or comments,
 * sending you to the appropriate function. skipChars skips all chars
 * inside brackets (with redirection to skipInComment) while skipComment
 * ignores brackets and prints all *)
let rec printBody ch =
   fprintf outFile "%c" ch;
   match ch with
   | '{' -> skipChars (Stream.next charStream)
   | '(' -> (match Stream.peek charStream with
            | Some('*') -> skipComment (Stream.next charStream)
            | _         -> printBody (Stream.next charStream))
   | _ -> printBody (Stream.next charStream)
and skipChars ch =
   match ch with
   | '}' -> fprintf outFile "%c" ch; printBody (Stream.next charStream)
   | '(' -> (match Stream.peek charStream with
            | Some('*') -> fprintf outFile "%c" '(';
                           skipInComment (Stream.next charStream)
            | _         -> skipChars (Stream.next charStream))
   | _ -> skipChars (Stream.next charStream)
and skipComment ch =
   match ch with
   | '*' -> fprintf outFile "%c" ch;
            (match Stream.peek charStream with
            | Some(')') -> printBody (Stream.next charStream)
            | _         -> skipComment (Stream.next charStream))
   | _   -> fprintf outFile "%c" ch;
            skipComment (Stream.next charStream)
and skipInComment ch =
   match ch with
   | '*' -> fprintf outFile "%c" ch;
            (match Stream.peek charStream with
            | Some(')') -> fprintf outFile "%c" ')';
                           skipChars (Stream.next charStream)
            | _         -> skipInComment (Stream.next charStream))
   | _   -> fprintf outFile "%c" ch;
            ;
            skipInComment (Stream.next charStream)

(* Prints all chars of the first section of a parser file (before %%).
 * It then passes control to the printBody function to strip ast parts *)
let rec printAll ch =
   fprintf outFile "%c" ch;
   match ch with
   | '%' -> (match Stream.peek charStream with
            | Some('%') -> printBody (Stream.next charStream)
            | None -> close_in input
            | _   -> printAll (Stream.next charStream))
   | _   -> printAll (Stream.next charStream)

(* Main function. Initiates printing functions and closes
 * the files when stream is empty (exception is raised) *)
let () =
   try
      printAll (Stream.next charStream)
   with e ->
      close_in input;
      close_out outFile
