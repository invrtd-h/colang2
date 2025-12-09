open! Core
open Util

let whitespace : unit Utf8.Parser.t =
  let run l =
    match l with
    | Utf8.C1 (' ' | '\r' | '\n' | '\t') :: tl -> tl, Ok ()
    | _ -> l, Or_error.errorf "Not a whitespace"
  in
  { run }
;;

let syntactic : Token.t Utf8.Parser.t =
  let run l =
    match l with
    | hd :: tl ->
      let token =
        List.Assoc.find Token.Syntactic.symbol_assoc ~equal:[%equal: Utf8.t list] [ hd ]
      in
      (match token with
       | Some token -> tl, Ok (Token.Syntactic token)
       | _ -> l, Or_error.errorf "No such tokens")
    | _ -> l, Or_error.errorf "Empty input"
  in
  { run }
;;

let braces : Token.t Utf8.Parser.t =
  let run l =
    match l with
    | (Utf8.C1 ('(' | ')' | '{' | '}' | '[' | ']') as c) :: tl ->
      let token =
        List.Assoc.find_exn
          ~equal:[%equal: Utf8.t list]
          Token.Syntactic.symbol_assoc
          [ c ]
      in
      let token : Token.t = Syntactic token in
      tl, Ok token
    | _ -> l, Or_error.errorf "Not in '(){}[]'"
  in
  { run }
;;

let lexid : Token.t Utf8.Parser.t =
  let rec run l =
    match l with
    | (Utf8.C3 _ as c) :: tl -> run' [ c ] tl
    | _ -> l, Or_error.errorf "Not an identifier"
  and run' acc l =
    match l with
    | (Utf8.C3 _ as c) :: tl -> run' (c :: acc) tl
    | _ -> l, Ok (Token.Id (List.rev acc))
  in
  { run }
;;

let lexer : Token.t list Utf8.Parser.t =
  let run l =
    match l with
    | _ -> l, Or_error.errorf "Unexpected token"
  in
  { run }
;;
