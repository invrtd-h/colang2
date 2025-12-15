open! Core
include module type of Value_intf.T
open Typ.Witness

val bool : bool -> bool_t t
val sint : (module Width with type t = 'w) -> Z.t -> ('w, signed_t) int_t t
val uint : (module Width with type t = 'w) -> Z.t -> ('w, unsigned_t) int_t t

val uadd
  :  ('w, unsigned_t) int_t t
  -> ('w, unsigned_t) int_t t
  -> ('w, unsigned_t) int_t t
