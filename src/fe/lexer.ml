let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]

open Parser

let rec read buf =
  match%sedlex buf with
  | white_space -> read buf
  | "true" -> TRUE
  | "false" -> FALSE
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '?' -> QUESTION
  | '!' -> EXCLAMATION
  | number -> INT (int_of_string (Sedlexing.Utf8.lexeme buf))
  | id_start, Star id_continue -> begin
    match Sedlexing.Utf8.lexeme buf with
    | "유링게슝한" -> IF
    | "안유링게슝" -> ELSE
    | "아니세상에" -> LET1
    | "자기가" -> JAGIGA
    | "라는" -> RANUN
    | "이라는" -> IRANUN
    | "사람인데" -> SARAMINDE
    | "을" -> EUL
    | "를" -> REUL
    | "했대" -> HETE
    | s -> ID s
    end
  | eof -> EOF
  | _ -> failwith "Unexpected Character"