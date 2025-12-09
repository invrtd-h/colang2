open! Core
open Util

module Keyword = struct
  include Token_intf.Keyword

  let symbol_assoc =
    [ "\xea\xb3\x84\xec\x95\xbd\xec\x84\x9c", Contract
    ; "\xea\xb9\xa0\xeb\x8b\xa4\xea\xb5\xac", Public
    ]
    |> List.map ~f:(fun (k, v) -> Utf8.parse k, v)
  ;;
end

module Syntactic = struct
  include Token_intf.Syntactic

  let symbol_assoc =
    [ "(", Lparen
    ; ")", Rparen
    ; "{", Lbrace
    ; "}", Rbrace
    ; "[", Lbracket
    ; "]", Rbracket
    ; "=", Equal
    ; "!", Exclam
    ]
    |> List.map ~f:(fun (k, v) -> Utf8.parse k, v)
  ;;
end

include Token_intf.Token
