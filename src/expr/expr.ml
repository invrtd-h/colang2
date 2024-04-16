exception AssignError of string
exception NotImplemented

module IntMap = Map.Make(Int)

type 'a int_map = 'a IntMap.t

module Vec = BatDynArray
type 'a vec = 'a Vec.t
type 'a arr = 'a Array.t

type expr = 
  | Value of value
  | Id of int
  | Op of expr * expr * expr * op
  | FunE of expr * int
  | Apply of expr * expr
  | Assign of int * expr
  | Let of int * expr * expr
  | LetRec of int * expr * int * expr
  | IfE of expr * expr * expr
  | WhileE of expr * expr
  | VecE of expr list
  | TupleE of expr arr
and value = 
  | UnitV
  | IntV of int
  | BoolV of bool
  | FunV of expr * env * int
  | VecV of value vec
  | TupleV of value arr
and env = {
  data : int int_map
} and op = value -> value -> value -> value

let new_env = {
  data = IntMap.empty
}

type mem = value Mem.t

let unit_e = Value UnitV
let value_e value = Value value
let int_e n = Value (IntV n)
let id_e id = Id id
let fun_e body id = FunE (body, id)
let apply fn arg = Apply (fn, arg)
let assign_e id e = Assign (id, e)
let let_e id expr next = Let (id, expr, next)
let let_rec f_id body arg_id next = LetRec (f_id, body, arg_id, next)
let if_e flag t f = IfE (flag, t, f)
let while_e flag body = WhileE (flag, body)
let vec_e l = VecE l
let tuple_e a = TupleE a

module Misc = struct
  let extract_fun : value -> expr * env * int
  = fun v -> 
    let [@warning "-partial-match"] FunV(expr, env, id) = v in
    expr, env, id
    
  let extract_bool : value -> bool
  = fun v ->
    let [@warning "-partial-match"] BoolV b = v in b
end

let binop_maker_iii : (int -> int -> int) -> (value -> value -> value -> value)
= fun f -> begin
    fun vl vr _ -> begin
      let [@warning "-partial-match"] IntV l = vl in
      let [@warning "-partial-match"] IntV r = vr in
      IntV (f l r)
    end
  end

let add_e l r = Op (l, r, unit_e, binop_maker_iii ( + ))
let sub_e l r = Op (l, r, unit_e, binop_maker_iii ( - ))
let mul_e l r = Op (l, r, unit_e, binop_maker_iii ( * ))
let div_e l r = Op (l, r, unit_e, binop_maker_iii ( / ))

let binop_maker_iib : (int -> int -> bool) -> (value -> value -> value -> value)
= fun f -> begin
    fun vl vr _ -> begin
      let [@warning "-partial-match"] IntV l = vl in
      let [@warning "-partial-match"] IntV r = vr in
      BoolV (f l r)
    end
  end
  
let lt_e l r = Op (l, r, unit_e, binop_maker_iib ( < ))

let vec_get vec_v idx_v _ = begin
  let [@warning "-partial-match"] VecV vec = vec_v in
  let [@warning "-partial-match"] IntV idx = idx_v in
  Vec.get vec idx
end

let vec_set vec_v idx_v value = begin
  let [@warning "-partial-match"] VecV vec = vec_v in
  let [@warning "-partial-match"] IntV idx = idx_v in
  let _ = Vec.set vec idx value in
  UnitV
end

let rec pret : expr -> env -> mem -> value
= fun expr env mem -> match expr with
  | Value v -> v
  | Id id ->
    let addr = env.data |> IntMap.find id in
    Mem.get mem addr
  | Op (a, b, c, op) ->
    let a = pret a env mem in
    let b = pret b env mem in
    let c = pret c env mem in
    op a b c
  | FunE (body, arg) -> FunV (body, env, arg)
  | Apply (fn, arg) -> 
    let (body, f_env, arg_id) = pret fn env mem |> Misc.extract_fun in
    let arg_val : value = pret arg env mem in
    let arg_addr : int = Mem.malloc mem in
    let _ = Mem.set mem arg_addr arg_val in
    let new_env : env = { data = f_env.data |> IntMap.add arg_id arg_addr } in
    let ret : value = pret body new_env mem in
    let _ = Mem.kill mem arg_addr in
    ret
  | Assign (id, expr) ->
    let addr = env.data |> IntMap.find id in
    let value = pret expr env mem in
    let _ = Mem.set mem addr value in
    UnitV
  | Let (var_id, init_expr, next_expr) ->
    let var_addr : int = Mem.malloc mem in
    let init_value : value = pret init_expr env mem in
    let _ = Mem.set mem var_addr init_value in
    let new_env : env = { data = env.data |> IntMap.add var_id var_addr } in
    let ret : value = pret next_expr new_env mem in
    let _ = Mem.kill mem var_addr in
    ret
  | LetRec (fun_id, body, arg_id, next_expr) ->
    let fun_addr : int = Mem.malloc mem in
    let rec funV = FunV (body, new_env, arg_id)
    and new_env : env = { data = env.data |> IntMap.add fun_id fun_addr } in
    let _ = Mem.set mem fun_addr funV in
    pret next_expr new_env mem
  | IfE (flag, t, f) ->
    let flag = pret flag env mem |> Misc.extract_bool in
    if flag then
      pret t env mem
    else
      pret f env mem
  | WhileE (flag, body) ->
    let _ = while let b = pret flag env mem |> Misc.extract_bool in b do 
      let _ = pret body env mem in ()
    done in 
    UnitV
  | VecE list ->
    let f = fun expr -> pret expr env mem in
    VecV (list |> List.map f |> Vec.of_list)
  | TupleE arr ->
    let f = fun expr -> pret expr env mem in
    TupleV (arr |> Array.map f)

let run : expr -> value
= fun expr -> pret expr new_env (Mem.create ())