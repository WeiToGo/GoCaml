open Scan

let filename = Sys.argv.(1)

let main () =
	let input = open_in filename in
	let filebuf = Lexing.from_channel input in
	try
		ignore(Scan.scan filebuf);
  		Printf.eprintf "VALID \n "
	with
	| Scan.Error msg ->
		Printf.eprintf " INVALID :%s%!" msg

let _ = main ()