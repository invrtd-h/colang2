open! Core

let () =
  let argv = Sys.get_argv () in
  let s = argv.(1) in
  let s = Std.string_dump s in
  Stdio.print_endline s
;;
