(*

  This file is part of GoCaml, a compiler that compiles Go-Lite (a subset of Go) to Java bytecode. 

  Copyright (C) 2015 by Deepanjan Roy, Wei Gao, Omar Gonzalez 


  GoCaml is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  GoCaml is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Foobar.  If not, see <http://www.gnu.org/licenses/>. 

  This code originated as a project for the COMP 520 class at McGill University
  in Winter 2015. Any subsequent COMP 520 student who is viewing this code must 
  follow the course rules and report any viewing and/or use of the code.

*)

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
