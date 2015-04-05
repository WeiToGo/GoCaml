let new_counter init = 
  let count = ref init in
  let next_count () = count := !count + 1; !count
  in
  next_count

module Int = struct type t = int let compare = compare end
