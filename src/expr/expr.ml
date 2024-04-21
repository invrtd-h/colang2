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
  | UnaryOp of expr * unary_op
  | BinOp of expr * expr * binop
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
}
and unary_op = value -> value
and binop = value -> value -> value
and op = value -> value -> value -> value

let new_env = {
  data = IntMap.empty
}

type mem = value Mem.t

module Misc = struct
  let extract_fun : value -> expr * env * int
  = fun v -> 
    let [@warning "-partial-match"] FunV(expr, env, id) = v in
    expr, env, id
    
  let extract_bool : value -> bool
  = fun v ->
    let [@warning "-partial-match"] BoolV b = v in b
end

module Op = struct
  let binop_maker_iii : (int -> int -> int) -> (value -> value -> value)
  = fun f -> begin
      fun vl vr -> begin
        let [@warning "-partial-match"] IntV l = vl in
        let [@warning "-partial-match"] IntV r = vr in
        IntV (f l r)
      end
    end
  
  let add = binop_maker_iii ( + )
  let sub = binop_maker_iii ( - )
  let mul = binop_maker_iii ( * )
  let div = binop_maker_iii ( / )
  
  let binop_maker_iib : (int -> int -> bool) -> (value -> value -> value)
  = fun f -> begin
      fun vl vr -> begin
        let [@warning "-partial-match"] IntV l = vl in
        let [@warning "-partial-match"] IntV r = vr in
        BoolV (f l r)
      end
    end
  
  let lt = binop_maker_iib ( < )
  
  let tup_get idx tup_v = begin
    let [@warning "-partial-match"] TupleV tup = tup_v in
    Array.get tup idx
  end
  
  let vec_get vec_v idx_v = begin
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
end

module Help = struct
  let unit = Value UnitV
  let value value = Value value
  let int n = Value (IntV n)
  let bool b = Value (BoolV b)
  let id id = Id id
  let binop lhs rhs op = BinOp (lhs, rhs, op)
  let op a b c o = Op (a, b, c, o)
  let fun' body id = FunE (body, id)
  let apply fn arg = Apply (fn, arg)
  let assign id e = Assign (id, e)
  let let' id expr next = Let (id, expr, next)
  let let_rec f_id body arg_id next = LetRec (f_id, body, arg_id, next)
  let if' flag t f = IfE (flag, t, f)
  let while' flag body = WhileE (flag, body)
  let vec l = VecE l
  let tuple a = TupleE a
  
  let add l r = BinOp (l, r, Op.add)
  let sub l r = BinOp (l, r, Op.sub)
  let mul l r = BinOp (l, r, Op.mul)
  let div l r = BinOp (l, r, Op.div)
  
  let lt l r = BinOp (l, r, Op.lt)
  
  let tup_get n tup = UnaryOp (tup, Op.tup_get n)
  
  let let_join : (expr -> expr) list -> expr -> expr
  = fun l e -> 
    let l = List.rev l in
    let rec aux : (expr -> expr) list -> expr -> expr
    = fun l e ->
      match l with
      | [] -> e
      | h :: t -> aux t (h e)
    in
    aux l e
end

let rec pret : expr -> env -> mem -> value
= fun expr env mem -> match expr with
  | Value v -> v
  | Id id ->
    let addr = env.data |> IntMap.find id in
    Mem.get mem addr
  | UnaryOp (e, op) ->
    let v = pret e env mem in
    op v
  | BinOp (lhs, rhs, op) ->
    let lhs = pret lhs env mem in
    let rhs = pret rhs env mem in
    op lhs rhs
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