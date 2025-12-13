open Typ.Witness

module T : sig
  type _ t = private
    | Bool : bool -> bool_t t
    | Sint : { n : Z.t } -> _ sint_t t
    | Uint : { n : Z.t } -> _ uint_t t
  [@@deriving variants]
end = struct
  type _ t =
    | Bool : bool -> bool_t t
    | Sint : { n : Z.t } -> _ sint_t t
    | Uint : { n : Z.t } -> _ uint_t t
  [@@deriving variants]
end
