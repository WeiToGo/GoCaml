open Scan
open Parser

(* let filename = Sys.argv.(1) *)

let main () =
	(* let input = open_in filename in *)
	let filebuf = Lexing.from_channel stdin in
	try
		Parser.program Scan.wrapped_scan filebuf;
  		Printf.eprintf "VALID \n ";
	with
	| Parser.Error ->
		Printf.eprintf " INVALID"

(* let _ = main () *)

let get_ast fname =
  let input = open_in fname in
  let filebuf = Lexing.from_channel input in
  Parser.program Scan.wrapped_scan filebuf
