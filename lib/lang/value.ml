include Value_intf.T
open Typ.Witness

let bool b : bool_t t = bool b

module Width (X : sig
    val n : int
  end) =
struct
  type t

  let n = X.n
end

let uint (type w) (module W : Width with type t = w) (x : Z.t) : 'w uint_t t =
  if Z.(x < zero || x >= shift_left one W.n) then failwith "what";
  uint ~n:x
;;

let sint (type w) (module W : Width with type t = w) (x : Z.t) : 'w sint_t t =
  let bound = Z.(shift_left one (Int.sub W.n 1)) in
  if Z.(x < neg bound || x >= bound) then failwith "what";
  sint ~n:x
;;

let uadd (_l : 'w uint_t t) (_r : 'w uint_t t) : 'w uint_t t = _l

module W256 = Width (struct
    let n = 256
  end)

let () = ignore (uint (module W256) Z.zero)
