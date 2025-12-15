open! Core
open Typ.Witness

module T : sig
  type _ t = private
    | Bool : bool -> bool_t t
    | Int : { n : Z.t } -> _ int_t t
  [@@deriving variants]
end = struct
  type _ t =
    | Bool : bool -> bool_t t
    | Int : { n : Z.t } -> _ int_t t
  [@@deriving variants]
end
