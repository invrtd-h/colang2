open! Core
open Util

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
    | Id of Utf8.t list
    | Operator of Utf8.t list
    | Keyword of Keyword.t
    | Syntactic of Syntactic.t
end
