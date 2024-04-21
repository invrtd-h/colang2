module Make :
  functor (Ord : Map.OrderedType) ->
    sig
      module BaseMap : sig type 'a t end
      type key = Ord.t
      type 'a base_map = private 'a BaseMap.t
      type 'a t = private { data : 'a base_map; size : int; }
      val empty : 'a t
      val add : key -> 'a -> 'a t -> 'a t
      val contains : key -> 'a t -> bool
      val find : key -> 'a t -> 'a
      val find_opt : key -> 'a t -> 'a option
      val size : 'a t -> int
      val to_base_map : 'a t -> 'a base_map
    end
