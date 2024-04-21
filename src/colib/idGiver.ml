let state = ref 0

let gen () = 
  let state' = !state in
  let () = state := state' + 1 in
  "$::" ^ string_of_int state'