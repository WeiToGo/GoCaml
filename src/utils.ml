let new_counter init = 
  let count = ref init in
  let next_count () = 
    let cur_count = !count in 
    count := !count + 1; cur_count
  in
  next_count

module Int = struct type t = int let compare = compare end
