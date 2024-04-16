let parse s =
  let revised_parser = 
    MenhirLib.Convert.Simplified.traditional2revised Parser.prog
  in
  let provider buf =
    let start, stop = Sedlexing.lexing_positions buf in
    let token = Lexer.read buf in
    token, start, stop
  in
  let lexbuf = Sedlexing.Utf8.from_string s in
  let revised_lexer () = provider lexbuf in
  revised_parser revised_lexer