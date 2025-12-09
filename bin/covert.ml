open! Core

let () =
  let argv = Sys.get_argv () in
  let s = argv.(1) in
  let s = Util.string_dump s in
  Stdio.print_endline s
;;
