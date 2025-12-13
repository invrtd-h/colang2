type t =
  | Bool
  | Int of
      { signed : bool
      ; size : int
      }

module Witness = struct
  type bool_t
  type 'width_witness sint_t
  type 'width_witness uint_t

  module type Width = sig
    type t

    val n : int
  end
end
