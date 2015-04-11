let new_counter init = 
  let count = ref init in
  let next_count () = 
    let cur_count = !count in 
    count := !count + 1; cur_count
  in
  next_count

module Int = struct type t = int let compare = compare end

let copy_file infilename outfilename = 
  let ic = open_in infilename in 
  let oc = open_out outfilename in  
  try (
    let rec rec_write () = 
      let line = input_line ic in 
      let () = output oc line 0 (String.length line) in 
      let () = output oc "\n" 0 1 in 
      rec_write () 
    in rec_write ())
  with End_of_file -> close_in ic; close_out oc; ()

let copy_from_staticlib infilename fname = 
  let src = Filename.concat (Filename.dirname Sys.argv.(0)) ("staticlib/" ^ fname) in 
  let dest = Filename.concat (Filename.dirname infilename) fname in 
  copy_file src dest