let usage_msg = "what"

let verbose = ref false

let anon_fun _ = ()

let speclist = [
  ("-sayhello", Arg.Set verbose, "say hello")
]

let () = Arg.parse speclist anon_fun usage_msg

let () = begin
  if !verbose then
    print_endline "Hello, World!"
end