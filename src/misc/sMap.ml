module Make (Ord : Map.OrderedType) = struct
  module BaseMap = Map.Make(Ord)
  
  type key = Ord.t
  type 'a base_map = 'a BaseMap.t
  type 'a t = {
    data : 'a BaseMap.t;
    size : int;
  }
  
  let empty : 'a t = {
    data = BaseMap.empty;
    size = 0;
  }
  
  let add : key -> 'a -> 'a t -> 'a t
  = fun key value map -> 
    let found = BaseMap.find_opt key map.data in
    let new_map = BaseMap.add key value map.data in
    match found with
    | Some _ -> { map with data = new_map }
    | None -> {
      data = new_map;
      size = map.size + 1;
    }
    
  let contains : key -> 'a t -> bool
  = fun key map -> BaseMap.find_opt key map.data |> Option.is_some
  
  let find : key -> 'a t -> 'a
  = fun key map -> BaseMap.find key map.data
    
  let find_opt : key -> 'a t -> 'a option
  = fun key map -> BaseMap.find_opt key map.data
    
  let size : 'a t -> int 
  = fun map -> map.size
  
  let to_base_map : 'a t -> 'a base_map
  = fun map -> map.data
  
end