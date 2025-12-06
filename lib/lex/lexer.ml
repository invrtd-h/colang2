open! Core

let extract (s : string) : (string * Token.t option) Or_error.t =
  let drop n = String.drop_prefix s n in
  let c = s.[0] in
  match c with
  | ' ' | '\r' | '\n' | '\t' -> Ok (drop 1, None)
  | '(' | ')' | '{' | '}' | '[' | ']' ->
    let token =
      List.Assoc.find_exn
        ~equal:String.equal
        Token.Syntactic.symbol_assoc
        (Char.to_string c)
    in
    let token : Token.t = Syntactic token in
    Ok (drop 1, Some token)
  | _ -> Or_error.errorf "Lex: Unexpected token (chr = %i)" (Char.to_int s.[0])
;;
