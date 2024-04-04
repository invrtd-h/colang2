exception AssignError of string
exception NotImplemented

module IntMap = Map.Make(Int)

type 'a int_map = 'a IntMap.t

type expr = 
  | UnitE
  | IntE of int
  | Id of int
  | Op of expr * expr * expr * (value -> value -> value -> value)
  | Assign of int * expr
  | FunE of expr * int
  | Apply of expr * expr
  | Seqn of expr * expr
and value = 
  | UnitV
  | IntV of int
  | FunV of expr * env * int
and env = {
  data : int int_map
}

let new_env = {
  data = IntMap.empty
}

type mem = value Mem.t

module Misc = struct
  let extract_fun : value -> expr * env * int
  = fun v -> 
    let [@warning "-partial-match"] FunV(expr, env, id) = v in
    expr, env, id
end

let binop_maker : (int -> int -> int) -> (value -> value -> value -> value)
= fun f -> begin
    fun vl vr _ -> begin
      let [@warning "-partial-match"] IntV l = vl in
      let [@warning "-partial-match"] IntV r = vr in
      IntV (f l r)
    end
  end

let add_e l r = Op (l, r, UnitE, binop_maker ( + ))
let sub_e l r = Op (l, r, UnitE, binop_maker ( - ))

let rec pret : expr -> env -> mem -> value
= fun expr env mem -> match expr with
  | UnitE -> UnitV
  | IntE n -> IntV n
  | Id id ->
    let addr = env.data |> IntMap.find id in
    Mem.get mem addr
  | Op (a, b, c, op) ->
    let a = pret a env mem in
    let b = pret b env mem in
    let c = pret c env mem in
    op a b c
  | Assign (id, expr) ->
    let addr = env.data |> IntMap.find id in
    let value = pret expr env mem in
    let _ = Mem.set mem addr value in
    UnitV
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
  | Seqn (e1, e2) ->
    let _ = pret e1 env mem in
    pret e2 env mem

let run : expr -> value
= fun expr -> pret expr new_env (Mem.create ())