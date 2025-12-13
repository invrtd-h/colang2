include module type of Value_intf.T
open Typ.Witness

val bool : bool -> bool_t t
val sint : (module Width with type t = 'w) -> Z.t -> 'w sint_t t
val uint : (module Width with type t = 'w) -> Z.t -> 'w uint_t t
val uadd : 'w uint_t t -> 'w uint_t t -> 'w uint_t t
