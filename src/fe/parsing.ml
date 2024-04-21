module I = Parser.MenhirInterpreter
module S = MenhirLib.General

let pp_pos out { Ppxlib.pos_lnum; pos_cnum; pos_bol; _} =
  Format.fprintf out "line %d:%d" pos_lnum (pos_cnum - pos_bol)

let state checkpoint : int =
  match Lazy.force (I.stack checkpoint) with
  | S.Nil ->
      (* Hmm... The parser is in its initial state. Its number is
         usually 0. This is a BIG HACK. TEMPORARY *)
      0
  | S.Cons (Element (s, _, _, _), _) ->
      I.number s

let handle_syntax_error lexbuf env =
  let message =
    match Parser_messages.message (state env) with
    | exception Not_found ->
        "Syntax error"
    | msg ->
        msg
  in
  Format.fprintf Format.err_formatter "%s %a\n%!" message pp_pos
    (fst @@ Sedlexing.lexing_positions lexbuf)

let rec loop next_token lexbuf (checkpoint : L1lang.l1expr I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
    let token = next_token () in
    let checkpoint = I.offer checkpoint token in
    loop next_token lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
    let checkpoint = I.resume checkpoint in
    loop next_token lexbuf checkpoint
  | I.HandlingError env ->
    let message =
      match Parser_messages.message (state env) with
      | exception Not_found ->
          "Syntax error"
      | msg ->
          msg
    in
    let () = Format.fprintf Format.err_formatter "%s %a\n%!" message pp_pos
    (fst @@ Sedlexing.lexing_positions lexbuf) in
    failwith ""
  | I.Accepted ast -> ast
  | I.Rejected ->
      (* Cannot happen as we stop at syntax error immediatly *)
      assert false

let process lexbuf =
  let lexer = Lexer.lexer lexbuf in
  try
    loop lexer lexbuf
      (Parser.Incremental.prog (fst @@ Sedlexing.lexing_positions lexbuf))
  with Lexer.LexError (pos, msg) ->
    let () = Format.fprintf Format.err_formatter "lexing error : %s at %a%!" msg
      pp_pos pos in
    failwith ""

let parse s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  process lexbuf