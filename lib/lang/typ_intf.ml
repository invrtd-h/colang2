type t =
  | Bool
  | Sint of { size : int }
  | UInt of { size : int }

module Witness = struct
  type bool_t
  type signed_t
  type unsigned_t
  type ('width_witness, 'sign_witness) int_t

  module type Width = sig
    type t

    val n : int
  end
end
