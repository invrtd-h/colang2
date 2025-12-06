open Core

module Keyword : sig
  include module type of Token_intf.Keyword

  val symbol_assoc : (string, t) List.Assoc.t
end

module Syntactic : sig
  include module type of Token_intf.Syntactic

  val symbol_assoc : (string, t) List.Assoc.t
end

include module type of Token_intf.Token
