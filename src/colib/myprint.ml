let list_to_string f l =
  l |> List.map f |> List.fold_left ( ^ ) ""
  
let arr_to_string f a =
  a |> Array.map f |> Array.fold_left ( ^ ) ""