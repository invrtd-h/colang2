let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]

exception LexError of Lexing.position * string

open Parser

let rec read buf =
  match%sedlex buf with
  | white_space -> read buf
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '[' -> LBRACK
  | ']' -> RBRACK
  | '?' -> QUESTION
  | "??" -> QUESQUES
  | '!' -> EXCLAMATION
  | "!!" -> EXCLAMEXCLAM
  | ':' -> COLON
  | ';' -> SEMICOLON
  | ',' -> COMMA
  | '_' -> UNDERSCORE
  | "->" -> ARROW
  | number -> INT (int_of_string (Sedlexing.Utf8.lexeme buf))
  | id_start, Star id_continue -> begin
    match Sedlexing.Utf8.lexeme buf with
    | "\u{c720}\u{b9c1}\u{ac8c}\u{c29d}\u{d55c}" -> IF   (* 유링게슝한 *)
    | "\u{c548}\u{c720}\u{b9c1}\u{ac8c}\u{c29d}" -> ELSE (* 안유링게슝 *)
    
    | "\u{cd98}\u{c7a3}" -> CHUNJAT                      (* 춘잣 *)
    
    | "\u{c544}\u{b2c8}\u{c138}\u{c0c1}\u{c5d0}" -> LET1 (* 아니세상에 *)
    | "\u{c790}\u{ae30}\u{ac00}" -> JAGIGA               (* 자기가 *)
    | "\u{b77c}\u{b294}" -> IRANUN                       (* 라는 *)
    | "\u{c774}\u{b77c}\u{b294}" -> IRANUN               (* 이라는 *)
    | "\u{c0ac}\u{b78c}\u{c778}\u{b370}" -> SARAMINDE    (* 사람인데 *)
    | "\u{c744}" -> REUL                                 (* 을 *)
    | "\u{b97c}" -> REUL                                 (* 를 *)
    | "\u{d588}\u{b300}" -> HETE                         (* 했대 *)
    
    | "\u{c544}" -> AH                                   (* 아 *)
    | "\u{c57c}" -> AH                                   (* 야 *)
    | "\u{ba39}\u{c5b4}\u{b77c}" -> MEOGEORA             (* 먹어라 *)
    
    | "\u{c2a4}\u{d0b5}\u{c774}\u{c57c}" -> SKIVIA       (* 스킵이야 *)
    | "\u{c2a4}\u{d0a4}\u{be44}\u{c57c}" -> SKIVIA       (* 스키비야 *)
    | "\u{c720}\u{b9ac}\u{acc4}\u{c218}" -> INTTYPE      (* 유리계수 *)
    | "\u{c870}\u{c774}\u{ace0}" -> JOYGO                (* 조이고 *)
    | "\u{bb49}\u{d0f1}\u{c774}" -> MTE                  (* 뭉탱이 *)
    | "\u{c5ec}\u{b7ec}\u{bd84}" -> YEOREOBUN            (* 여러분 *)
    | "\u{c740}" -> NEUN                                 (* 은 *)
    | "\u{b294}" -> NEUN                                 (* 는 *)
    | "\u{c77c}\u{ae4c}\u{c694}" -> ILKAYO               (* 일까요 *)
    
    | "\u{c778}" -> IN                                   (* 인 *)
    | "\u{c911}\u{c5d0}\u{b294}" -> JUNGENEUN            (* 중에는 *)
    | "\u{c544}\u{bb34}\u{b9ac}" -> AMURI                (* 아무리 *)
    | "\u{b77c}\u{b3c4}" -> IRADO                        (* 라도 *)
    | "\u{c774}\u{b77c}\u{b3c4}" -> IRADO                (* 이라도 *)
    | "\u{d560}" -> HAL                                  (* 할 *)
    | "\u{c218}\u{ac00}" -> SUGA                         (* 수가 *)
    | "\u{c5c6}\u{b2e8}\u{b2e4}" -> UPDANDA              (* 없단다 *)
    
    | s -> ID s
    end
  | eof -> EOF
  | _ -> failwith "Unexpected Character"
  
let lexer buf =
  Sedlexing.with_tokenizer read buf