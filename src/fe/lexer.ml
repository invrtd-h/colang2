let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]

open Parser

let rec read buf =
  match%sedlex buf with
  | white_space -> read buf
  | number -> INT (int_of_string (Sedlexing.Utf8.lexeme buf))
  | id_start, Star id_continue -> ID (Sedlexing.Utf8.lexeme buf)
  | '(' -> LPAREN
  | ')' -> RPAREN
  | eof -> EOF
  | _ -> failwith "Unexpected Character"