open L1type

exception NotImplemented

exception TypeError of string

module StrTbl = Hashtbl.Make(String)
module StrSMap = Colib.SMap.Make(String)

type 'a str_smap = 'a StrSMap.t
type 'a arr = 'a Array.t

type l1expr =
  | UnitE
  | IntE of int
  | BoolE of bool
  | Id of string
  | UnaryOp of l1expr * Expr.unary_op * unop_tv
  | BinOp of l1expr * l1expr * Expr.binop * binop_tv
  | Op of l1expr * l1expr * l1expr * Expr.op * optype
  | FunE of arg_pattern * type_expr option * l1expr
  | Apply of l1expr * l1expr
  | Assign of string * l1expr
  | Let of let_pattern * l1expr * l1expr
  | LetRec of string * arg_pattern * l1expr * type_expr * l1expr
  | Seqn of l1expr * l1expr
  | IfE of l1expr * l1expr * l1expr
  | WhileE of l1expr * l1expr
  | VecE of l1expr list
  | TupleE of l1expr list
  | VariantDef of string * (string * type_expr) list * l1expr
  [@@deriving show]
and unop_tv = l1type -> l1type option
and binop_tv = l1type -> l1type -> l1type option
and optype = l1type * l1type * l1type * l1type
and let_pattern = 
  | UnderscoreLP
  | VarIdLP of string * type_expr option
  | AliasNameLP of let_pattern * string (* pattern as value-name *)
  | TupleLP of let_pattern list (* pattern, pattern, .. *)
  | ConstrLP of string * let_pattern (* constructor pattern *)
and arg_pattern = 
  | UnitAP
  | VarIdAP of string * type_expr
  | TupleAP of arg_pattern list

module E = struct 
  let unit = UnitE
  let int n = IntE n
  let bool b = BoolE b
  let id x = Id x
  let unary_op e op verifier = UnaryOp (e, op, verifier)
  let binop l r op verifier = BinOp (l, r, op, verifier)
  let fn pat ret_type body = FunE (pat, ret_type, body)
  let apply f arg = Apply (f, arg)
  let assign id e = Assign (id, e)
  let let' pattern body next = Let (pattern, body, next)
  let let_rec fn_id arg_pat body ret_type next = 
    LetRec (fn_id, arg_pat, body, ret_type, next)
  let seqn l r = Seqn (l, r)
  let if' flag t f = IfE (flag, t, f)
  let vec l = VecE l
  let tuple l = TupleE l
  let while' flag body = WhileE (flag, body)
  let variant_def vari_name vari_def next = VariantDef (vari_name, vari_def, next)
end
  
module Lp = struct
  let underscore = UnderscoreLP
  let var_id id var_type = VarIdLP (id, var_type)
  let alias_name pattern alias = AliasNameLP (pattern, alias)
  let tuple list = TupleLP list
end

module Ap = struct
  let unit = UnitAP
  let var_id var_id var_type = VarIdAP (var_id, var_type)
  let tuple list = TupleAP list
end

module Op = struct
  let add l r = BinOp (l, r, Expr.Op.add, fun _ _ -> Some T.int)
  let mul l r = BinOp (l, r, Expr.Op.mul, fun _ _ -> Some T.int)
  
  let tup_get idx e = UnaryOp (e, Expr.Op.tup_get idx, fun t -> 
    match t |> T.as_tuple with
    | Ok a -> Array.get a idx |> Option.some
    | Error () -> None
  )
end
  
let toplevel_join : (l1expr -> l1expr) list -> l1expr -> l1expr
= fun l e -> 
  let l = List.rev l in
  let rec aux : (l1expr -> l1expr) list -> l1expr -> l1expr
  = fun l e ->
    match l with
    | [] -> e
    | h :: t -> aux t (h e)
  in
  aux l e
  
module TupleBuilder = struct
  let join_rev l r = match l |> TE.as_tuple with
  | Ok data -> TE.tuple (r :: data)
  | _ -> TE.tuple [r; l]
  
  let rev t = match t |> TE.as_tuple with
  | Ok data -> TE.tuple (data |> List.rev)
  | _ -> failwith "`TupleBuilder.rev` should get TupleTE type as arg"
end

let rec assign_let_pat : let_pattern -> l1type -> t_env -> (string * l1type) list
= fun pat type_to_assign t_env -> begin
  match pat with
  | UnderscoreLP -> []
  | VarIdLP (var_id, None) -> [(var_id, type_to_assign)]
  | VarIdLP (var_id, Some type_expr) ->
    let l1type = TEnv.pret_type type_expr t_env in
    let () = assert (type_to_assign <:: l1type) in
    [(var_id, l1type)]
  | AliasNameLP (pat, alias_id) ->
    let inner = assign_let_pat pat type_to_assign t_env in
    (alias_id, type_to_assign) :: inner
  | TupleLP pat_data -> begin
    match type_to_assign |> T.as_tuple with
    | Ok type_data -> 
      let type_data = Array.to_list type_data in
      let mapped = 
        let mapper = fun pat t -> assign_let_pat pat t t_env in
        List.map2 mapper pat_data type_data in
      List.flatten mapped
    | _ -> failwith "The Assigned Type is Not a Tuple"
  end
  | ConstrLP (_constructor_name, _pat) ->
    failwith "unimplemented : assign_let_pat"
end

let compile_let_pat : let_pattern -> l1expr -> (string * l1expr) list
= fun pat defining_expr -> begin
  let rec aux : let_pattern -> string -> l1expr -> (string * l1expr) list
  = fun pat top_name defining_expr -> match pat with
    | UnderscoreLP -> []
    | VarIdLP (var_id, _) -> [(var_id, defining_expr)]
    | AliasNameLP (pat, alias_name) ->
      let l = aux pat top_name (E.id alias_name) in
      (alias_name, defining_expr) :: l
    | TupleLP data ->
      (* {let (l1, l2) = z} -> let l0 = z; let l1 = z.0; let l2 = z.1 *)
      let tuple_name = Colib.IdGiver.gen () in
      let f = fun idx pat -> aux pat tuple_name (Op.tup_get idx (E.id tuple_name)) in
      let data = List.mapi f data in
      (tuple_name, defining_expr) :: List.flatten data
    | _ -> failwith "unimplemented : compile_let_pat"
  in
  aux pat (Colib.IdGiver.gen ()) defining_expr
end

let rec extract_arg_pat_type : arg_pattern -> t_env -> (string * l1type) list
= fun pat t_env -> match pat with
  | UnitAP -> []
  | VarIdAP (var_id, type_expr) ->
    let l1type = TEnv.pret_type type_expr t_env in
    [(var_id, l1type)]
  | TupleAP data -> 
    data |> List.map (fun pat -> extract_arg_pat_type pat t_env) |> List.flatten

let rec pret_arg_pat_type : arg_pattern -> t_env -> l1type
= fun pat t_env -> match pat with
  | UnitAP -> T.unit
  | VarIdAP (_, t) -> TEnv.pret_type t t_env
  | TupleAP data -> 
    data |> List.map (fun pat -> pret_arg_pat_type pat t_env) 
    |> Array.of_list |> T.tuple

let type_check : l1expr -> unit
= fun expr ->
  let rec pret : l1expr -> t_env -> l1type
  = fun expr t_env ->
    let ( <:! ) l r
    = if l <:: r then 
        () 
      else  
        let report_elt = Printf.sprintf "%s is not the subtype of %s"
          (l1type_to_string l) (l1type_to_string r) in
        failwith report_elt
      in
    match expr with
    | UnitE -> T.unit
    | IntE _ -> T.int
    | BoolE _ -> T.bool
    | Id x -> t_env |> TEnv.find_var x
    | UnaryOp (e, _, verifier) ->
      let t = pret e t_env in
      let tret = verifier t in
      Option.get tret
    | BinOp (lhs, rhs, _, verifier) ->
      let tl = pret lhs t_env in
      let tr = pret rhs t_env in
      let tret = verifier tl tr in
      Option.get tret
    | Op (a, b, c, _, op) -> 
      let ta = pret a t_env in
      let tb = pret b t_env in
      let tc = pret c t_env in
      let ta_expect, tb_expect, tc_expect, tret = op in
      let () = ta <:! ta_expect in
      let () = tb <:! tb_expect in
      let () = tc <:! tc_expect in
      tret
    | FunE (pat, ret_type, body) ->
      begin
        let pat_info = extract_arg_pat_type pat t_env in
        let arg_type = pret_arg_pat_type pat t_env in
        let new_t_env = t_env |> TEnv.add_var_list pat_info in
        let body_type = pret body new_t_env in
        match ret_type with
        | Some ret_expect ->
          let ret_expect = TEnv.pret_type ret_expect t_env in
          let () = body_type <:! ret_expect in
          T.fn arg_type ret_expect
        | None -> T.fn arg_type body_type
      end
    | Apply (fn, arg) -> 
      let fn_type = pret fn t_env in
      begin
        match fn_type |> T.as_fun with
        | Ok (arg_type_expect, ret_type) -> 
          let arg_type = pret arg t_env in
          let () = arg_type <:! arg_type_expect in
          ret_type
        | _ -> 
          let report_elt = 
            Printf.sprintf "%s is not the function type, which is not applicable"
            (l1type_to_string fn_type) in
          failwith report_elt
      end
    | Assign (var_name, value_new) -> 
      let value_type = pret value_new t_env in
      let () = value_type <:! TEnv.find_var var_name t_env in
      T.unit
    | Let (pat, var_def, next) -> 
      let var_type = pret var_def t_env in
      let pat_info = assign_let_pat pat var_type t_env in
      let new_t_env = t_env |> TEnv.add_var_list pat_info in
      pret next new_t_env
    | LetRec (fn_name, pat, body, ret_type, next) -> 
      let pat_info = extract_arg_pat_type pat t_env in
      let arg_type = pret_arg_pat_type pat t_env in
      let ret_type = TEnv.pret_type ret_type t_env in
      let new_t_env = t_env |> TEnv.add_var_list pat_info in
      let new_t_env = new_t_env |> TEnv.add_var fn_name (T.fn arg_type ret_type) in
      let body_type = pret body new_t_env in
      let () = body_type <:! ret_type in
      pret next new_t_env
    | Seqn (l, r) -> 
      let _ = pret l t_env in
      pret r t_env
    | IfE (flag, t, f) -> 
      let flag_type = pret flag t_env in
      if flag_type <:: T.bool then
        let t_type = pret t t_env in
        let f_type = pret f t_env in
        let () = if t_type != f_type then
          let report_elt = 
            Printf.sprintf "true-type %s is not equal to false-type %s"
            (l1type_to_string t_type) (l1type_to_string f_type) in
          failwith report_elt
        in
        t_type
      else
        let () = flag_type <:! T.bool in
        T.bottom
    | WhileE (flag, _) -> 
      let flag_type = pret flag t_env in
      let () = flag_type <:! T.bool in
      T.unit
    | VecE l -> 
      let f = fun node -> pret node t_env in
      let _l = l |> List.map f in
      raise NotImplemented
    | TupleE arr -> 
      let f = fun node -> pret node t_env in
      arr |> List.map f |> Array.of_list |> T.tuple
    | VariantDef (name, variant_info, next) ->
      let f = fun t -> TEnv.pret_type t t_env in
      let mapping = StrTbl.create (List.length variant_info) in
      let () = variant_info 
      |> List.iter (fun (name, t) -> StrTbl.add mapping name (f t)) in
      let new_type = T.variant name mapping in
      let new_t_env = t_env |> TEnv.add_type name new_type in
      pret next new_t_env
  in
  let _ = pret expr TEnv.empty in
  ()


let compile : l1expr -> Expr.expr
= fun expr ->
  let idalloc : string -> int str_smap -> int str_smap * int
  = fun name env ->
    if env |> StrSMap.contains name then
      env, env |> StrSMap.find name
    else
      let new_id = env |> StrSMap.size in
      let new_env = env |> StrSMap.add name new_id in
      new_env, new_id
  in
  let rec aux : l1expr -> int str_smap -> Expr.expr
  = fun expr env -> 
    match expr with
    | UnitE -> Expr.Help.unit
    | IntE n -> Expr.Help.int n
    | BoolE b -> Expr.Help.bool b
    | Id id -> 
      let id = env |> StrSMap.find id in
      Expr.Help.id id
    | UnaryOp (e, op, _) ->
      Expr.Help.unary_op (aux e env) op
    | BinOp (lhs, rhs, op, _) ->
      Expr.Help.binop (aux lhs env) (aux rhs env) op
    | Op (a, b, c, op, _) -> 
      Expr.Help.op (aux a env) (aux b env) (aux c env) op
    | Let (pat, body, next) ->
      let env = ref env in
      let l : (Expr.expr -> Expr.expr) list ref = ref [] in
      
      let f : (string * l1expr) -> unit
      = fun (str_id, expr) -> begin
        let new_env, id = !env |> idalloc str_id in
        let () = env := new_env in
        let expr : Expr.expr = aux expr !env in
        l := Expr.Help.let' id expr :: !l
      end in
      
      let l' = compile_let_pat pat body in
      (* example: l = ["x", (2, 3); "y", "x".0; "z", "x".1] *)
      let () = List.iter f l' in
      let () = l := List.rev !l in
      Expr.Help.let_join !l (aux next !env)
    | Seqn (l, r) -> Expr.Help.seqn (aux l env) (aux r env)
    | TupleE l -> 
      l |> List.map (fun elem -> aux elem env) 
      |> Array.of_list
      |> Expr.Help.tuple
    | _ -> failwith "unimplemented : compile"
  in
  let _ = type_check expr in
  aux expr StrSMap.empty

let compile_and_run node = node |> compile |> Expr.run

