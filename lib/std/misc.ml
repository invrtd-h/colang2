open! Core

let string_dump s =
  let l = String.to_list s in
  let l = List.map l ~f:Char.to_int in
  let l =
    List.map l ~f:(fun n ->
      let s = Int.Hex.to_string n in
      let s = String.chop_prefix_exn ~prefix:"0x" s in
      {|\x|} ^ s)
  in
  let s = String.concat l in
  s
;;
