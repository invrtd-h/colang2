exception NotImplemented

exception TypeError of string

module StrSet = Set.Make(String)
module StrMap = Map.Make(String)

type str_set = StrSet.t
type 'a str_map = 'a StrMap.t
type 'a arr = 'a Array.t

type l1type =
  | UnitT
  | IntT
  | BoolT
  | FunT of l1type * l1type
  | VecT of l1type
  | TupleT of l1type arr
  | RecordT of (string * l1type) list
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
  | BottomT -> "Bottom"
  
let unit_t = UnitT
let int_t = IntT
let bool_t = BoolT
let fun_t arg ret = FunT (arg, ret)
let vec_t d = VecT d
let tuple_t arr = TupleT arr
let record_t : (string * l1type) list -> l1type
= fun l -> 
  let l = l |> List.sort (fun (id1, _) (id2, _) -> compare id1 id2) in
  RecordT l
let bottom_t = BottomT

let unit_te = UnitTE
let int_te = IntTE
let bool_te = BoolTE
let fun_te arg ret = FunTE (arg, ret)
let vec_te d = VecTE d
let tuple_te arr = TupleTE arr
let record_te : (string * type_expr) list -> type_expr
= fun l -> 
  let l = l |> List.sort (fun (id1, _) (id2, _) -> compare id1 id2) in
  RecordTE l
let bottom_te = BottomTE
let typeid_te x = TypeId x

type l1expr =
  | UnitE
  | IntE of int
  | BoolE of bool
  | Id of string
  | Op of ast_node * ast_node * ast_node * Expr.op * optype
  | FunE of arg_pattern * type_expr option * ast_node
  | Apply of ast_node * ast_node
  | Assign of string * ast_node
  | Let of let_pattern * ast_node * ast_node
  | LetRec of string * arg_pattern * ast_node * type_expr * ast_node
  | IfE of ast_node * ast_node * ast_node
  | WhileE of ast_node * ast_node
  | VecE of ast_node list
  | TupleE of ast_node arr
and optype = l1type * l1type * l1type * l1type
and ast_node = {
  expr : l1expr;
  info : (Lexing.position * Lexing.position) option;
}
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

let fresh : l1expr -> ast_node
= fun expr -> { expr; info = None; }

let make : Lexing.position * Lexing.position -> l1expr -> ast_node
= fun info expr -> { expr; info = Some info; }

let unit_e = UnitE
let int_e n = IntE n
let bool_e b = BoolE b
let id_e x = Id x
let fun_e pat ret_type body = FunE (pat, ret_type, body)
let apply_e f arg = Apply (f, arg)
let let_e pattern body next = Let (pattern, body, next)
let let_rec_e fn_id arg_pat body ret_type next = 
  LetRec (fn_id, arg_pat, body, ret_type, next)
let if_e flag t f = IfE (flag, t, f)
  
let underscore_lp = UnderscoreLP
let var_id_lp var_id var_type = VarIdLP (var_id, var_type)
let alias_name_lp pattern alias = AliasNameLP (pattern, alias)
let tuple_lp list = TupleLP list

let unit_ap = UnitAP
let var_id_ap var_id var_type = VarIdAP (var_id, var_type)
let tuple_ap list = TupleAP list
  
let toplevel_join : (ast_node -> ast_node) list -> ast_node -> ast_node
= fun l e -> 
  let l = List.rev l in
  let rec aux : (ast_node -> ast_node) list -> ast_node -> ast_node
  = fun l e ->
    match l with
    | [] -> e
    | h :: t -> aux t (h e)
  in
  aux l e
  
module TupleBuilder = struct
  let join_rev l r = match l with
  | TupleTE data -> tuple_te (r :: data)
  | _ -> TupleTE [r; l]
  
  let rev t = match t with
  | TupleTE data -> TupleTE (data |> List.rev)
  | _ -> failwith "`TupleBuilder.rev` should get TupleTE type as arg"
end

module TEnv = struct 
  type t_env = {
    vars : l1type str_map;
    aliases : l1type str_map;
    type_binds : str_set;
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
    | UnitTE -> unit_t
    | IntTE -> int_t
    | BoolTE -> bool_t
    | FunTE (a, b) -> fun_t (pret_type a t_env) (pret_type b t_env)
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
    match type_to_assign with
    | TupleT type_data -> 
      let type_data = Array.to_list type_data in
      let mapped = 
        let mapper = fun pat t -> assign_let_pat pat t t_env in
        List.map2 mapper pat_data type_data in
      List.flatten mapped
    | _ -> failwith "The Assigned Type is Not a Tuple"
  end
  | ConstrLP (_constructor_name, _pat) ->
    failwith "unimplemented"
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
  | UnitAP -> unit_t
  | VarIdAP (_, t) -> TEnv.pret_type t t_env
  | TupleAP data -> 
    data |> List.map (fun pat -> pret_arg_pat_type pat t_env) 
    |> Array.of_list |> tuple_t

let type_check : ast_node -> string list
= fun node ->
  let report : string list ref = ref [] in
  let rec pret : ast_node -> t_env -> l1type
  = fun node t_env ->
    let { expr; _ } = node in
    let ( <:! ) l r
    = if l <:: r then 
        () 
      else  
        let report_elt = Printf.sprintf "%s is not the subtype of %s"
          (l1type_to_string l) (l1type_to_string r) in
        report := report_elt :: !report
      in
    match expr with
    | UnitE -> UnitT
    | IntE _ -> IntT
    | BoolE _ -> BoolT
    | Id x -> t_env |> TEnv.find_var x
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
          fun_t arg_type ret_expect
        | None -> fun_t arg_type body_type
      end
    | Apply (fn, arg) -> 
      let fn_type = pret fn t_env in
      begin
        match fn_type with
        | FunT (arg_type_expect, ret_type) -> 
          let arg_type = pret arg t_env in
          let () = arg_type <:! arg_type_expect in
          ret_type
        | _ -> 
          let report_elt = 
            Printf.sprintf "%s is not the function type, which is not applicable"
            (l1type_to_string fn_type) in
          let () = report := report_elt :: !report in
          bottom_t
      end
    | Assign (var_name, value_new) -> 
      let value_type = pret value_new t_env in
      let () = value_type <:! TEnv.find_var var_name t_env in
      unit_t
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
      let new_t_env = new_t_env |> TEnv.add_var fn_name (fun_t arg_type ret_type) in
      let body_type = pret body new_t_env in
      let () = body_type <:! ret_type in
      pret next new_t_env
    | IfE (flag, t, f) -> 
      let flag_type = pret flag t_env in
      if flag_type <:: bool_t then
        let t_type = pret t t_env in
        let f_type = pret f t_env in
        let () = if t_type != f_type then
          let report_elt = 
            Printf.sprintf "true-type %s is not equal to false-type %s"
            (l1type_to_string t_type) (l1type_to_string f_type) in
          report := report_elt :: !report
        in
        t_type
      else
        let () = flag_type <:! bool_t in
        bottom_t
    | WhileE (flag, _) -> 
      let flag_type = pret flag t_env in
      let () = flag_type <:! bool_t in
      unit_t
    | VecE l -> 
      let f = fun node -> pret node t_env in
      let _l = l |> List.map f in
      raise NotImplemented
    | TupleE arr -> 
      let f = fun node -> pret node t_env in
      arr |> Array.map f |> tuple_t
  in
  let _ = pret node TEnv.empty in
  !report  
    
        
        
        