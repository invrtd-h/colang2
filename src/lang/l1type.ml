module StrSet = Set.Make(String)
module StrMap = Map.Make(String)
module StrTbl = Hashtbl.Make(String)

type 'a str_map = 'a StrMap.t
type 'a str_tbl = 'a StrTbl.t
type 'a arr = 'a Array.t

type l1type =
  | UnitT
  | IntT
  | BoolT
  | FunT of l1type * l1type
  | VecT of l1type
  | TupleT of l1type arr
  | RecordT of (string * l1type) list
  | VariantT of string * l1type str_tbl
  | BottomT
  
type type_expr =
  | UnitTE
  | IntTE
  | BoolTE
  | FunTE of type_expr * type_expr
  | VecTE of type_expr
  | TupleTE of type_expr list
  | RecordTE of (string * type_expr) list
  | BottomTE
  | TypeId of string
  
let rec ( <:: ) : l1type -> l1type -> bool
= fun l r -> match l, r with
  | BottomT, _ | UnitT, UnitT | IntT, IntT | BoolT, BoolT -> true
  | FunT (la, lr), FunT (ra, rr) -> (la <:: ra) && (rr <:: lr)
  | VecT vl, VecT vr -> vl <:: vr
  | TupleT la, TupleT ra -> 
    if Array.length la = Array.length ra then
      Array.for_all2 ( <:: ) la ra
    else false
  | _, _ -> false

let rec l1type_to_string : l1type -> string
= function
  | UnitT -> "Unit"
  | IntT -> "Int"
  | BoolT -> "Bool"
  | FunT (arg, ret) -> l1type_to_string arg ^ " -> " ^ l1type_to_string ret
  | VecT v -> "Vec[" ^ l1type_to_string v ^ "]"
  | TupleT arr -> 
    let arr = arr |> Array.map l1type_to_string in
    "(" ^ (Array.fold_left (fun a b -> a ^ ", " ^ b) "" arr) ^ ")"
  | RecordT recs ->
    let recs = recs |> List.map (fun (name, t) -> name ^ ": " ^ l1type_to_string t) in
    "{" ^ (List.fold_left (fun a b -> a ^ ", " ^ b) "" recs) ^ "}"
  | VariantT (name, variants) -> 
    variants |> StrTbl.to_seq |> List.of_seq 
    |> List.sort (fun (l, _) (r, _) -> compare l r) 
    |> List.map (fun (name, t) -> name ^ ": " ^ l1type_to_string t)
    |> List.fold_left (fun a b -> a ^ " | " ^ b) (name ^ " -> ")
  | BottomT -> "Bottom"
  
module T = struct
  let unit = UnitT
  let int = IntT
  let bool = BoolT
  let fn arg ret = FunT (arg, ret)
  let vec d = VecT d
  let tuple l = TupleT l
  let record : (string * l1type) list -> l1type
  = fun l -> 
    let l = l |> List.sort (fun (id1, _) (id2, _) -> compare id1 id2) in
    RecordT l
  let variant name info = VariantT (name, info)
  let bottom = BottomT
  
  let as_tuple t = match t with
  | TupleT a -> Ok a
  | _ -> Error ()
  
  let as_fun t = match t with
  | FunT (a, r) -> Ok (a, r)
  | _ -> Error ()
end

module TE = struct
  let unit = UnitTE
  let int = IntTE
  let bool = BoolTE
  let fn arg ret = FunTE (arg, ret)
  let vec d = VecTE d
  let tuple arr = TupleTE arr
  let record : (string * type_expr) list -> type_expr
  = fun l -> 
    let l = l |> List.sort (fun (id1, _) (id2, _) -> compare id1 id2) in
    RecordTE l
  let bottom = BottomTE
  let typeid x = TypeId x
  
  let as_tuple te = match te with
  | TupleTE l -> Ok l
  | _ -> Error ()
end

module TEnv = struct 
  type t_env = {
    vars : l1type str_map;
    aliases : l1type str_map;
    type_binds : StrSet.t;
  } 
  
  let empty = {
    vars = StrMap.empty;
    aliases = StrMap.empty;
    type_binds = StrSet.empty;
  }
  
  let find_var : string -> t_env -> l1type
  = fun id env -> StrMap.find id env.vars
  
  let add_var : string -> l1type -> t_env -> t_env
  = fun id l1type env -> { env with vars = StrMap.add id l1type env.vars }
  
  let rec add_var_list : (string * l1type) list -> t_env -> t_env
  = fun list env -> match list with
  | [] -> env
  | (id, l1type) :: t -> 
    let new_env = add_var id l1type env in
    add_var_list t new_env
  
  let find_type : string -> t_env -> l1type
  = fun id env -> StrMap.find id env.aliases
  
  let rec pret_type : type_expr -> t_env -> l1type
  = fun type_expr t_env -> match type_expr with
    | UnitTE -> T.unit
    | IntTE -> T.int
    | BoolTE -> T.bool
    | FunTE (a, b) -> T.fn (pret_type a t_env) (pret_type b t_env)
    | TupleTE t -> 
      let f = fun t -> pret_type t t_env in 
      TupleT (t |> List.map f |> Array.of_list)
    | VecTE t -> VecT (pret_type t t_env)
    | RecordTE data -> 
      let f = fun (name, t) -> (name, pret_type t t_env) in
      RecordT (data |> List.map f)
    | BottomTE -> BottomT
    | TypeId id -> t_env |> find_type id
  
  let add_type : string -> l1type -> t_env -> t_env
  = fun id l1type env -> { env with aliases = StrMap.add id l1type env.aliases }
end

type t_env = TEnv.t_env