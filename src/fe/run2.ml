module I = Parser.MenhirInterpreter
module S = MenhirLib.General

exception ParserError of string
exception LexerError of string

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
    let my_state = state env in
    let message =
      match Parser_messages.message my_state with
      | exception Not_found ->
          "Syntax error"
      | msg -> msg
    in
    let s = Format.asprintf "Parser Error: %s at %a (Parser State: %d)\n%!" 
    message pp_pos 
    (fst @@ Sedlexing.lexing_positions lexbuf) my_state in
    raise @@ ParserError s
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
    let s = Format.asprintf "lexing error : %s at %a%!" msg pp_pos pos in
    raise @@ LexerError s

let parse s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  process lexbuf
  
let typecheck s = 
  L1lang.type_check @@ parse @@ s
  
let compile s =
  L1lang.compile @@ parse @@ s
  
let run s =
  L1lang.compile_and_run @@ parse @@ s