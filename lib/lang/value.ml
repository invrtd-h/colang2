open! Core
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

let uint (type w) (module W : Width with type t = w) (x : Z.t) : ('w, unsigned_t) int_t t =
  let mask = Z.(shift_left one W.n - one) in
  let x = Z.(logand x mask) in
  int ~n:x
;;

let sint (type w) (module W : Width with type t = w) (x : Z.t) : ('w, signed_t) int_t t =
  let width = Z.(shift_left one W.n) in
  let mask = Z.(width - one) in
  let x = Z.(logand x mask) in
  let signbit = Z.(shift_left one (Int.( - ) W.n 1)) in
  let x = if Z.equal (Z.logand x signbit) Z.zero then x else Z.(x - width) in
  int ~n:x
;;

let uadd (_l : ('w, unsigned_t) int_t t) (_r : ('w, unsigned_t) int_t t)
  : ('w, unsigned_t) int_t t
  =
  let[@warning "-8"] (Int _) = _l in
  let[@warning "-8"] (Int _) = _r in
  _l
;;

module W256 = Width (struct
    let n = 256
  end)

let () = ignore (uint (module W256) Z.zero)
