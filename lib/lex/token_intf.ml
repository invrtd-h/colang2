open! Core

module Keyword = struct
  type t =
    | Contract
    | Public
  [@@deriving string]
end

module Syntactic = struct
  type t =
    | Lparen
    | Rparen
    | Lbrace
    | Rbrace
    | Lbracket
    | Rbracket
    | Equal
    | Exclam
  [@@deriving string]
end

module Token = struct
  type t =
    | Int of Z.t
    | Id of string
    | Operator of string
    | Keyword of Keyword.t
    | Syntactic of Syntactic.t
end
