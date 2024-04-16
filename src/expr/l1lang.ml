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

type l1expr =
  | UnitE
  | IntE of int
  | BoolE of bool
  | Id of string
  | Op of ast_node * ast_node * ast_node * Expr.op * optype
  | FunE of ast_node * string * l1type * l1type option
  | Apply of ast_node * ast_node
  | Assign of string * ast_node
  | Let of string * l1type option * ast_node * ast_node
  | LetRec of string * ast_node * string * l1type * l1type * ast_node
  | IfE of ast_node * ast_node * ast_node
  | WhileE of ast_node * ast_node
  | VecE of ast_node list
  | TupleE of ast_node arr
and optype = l1type * l1type * l1type * l1type
and ast_node = {
  expr : l1expr;
  info : Lexing.position * Lexing.position;
}

let fresh_node : l1expr -> ast_node
= fun expr -> { expr; info = Lexing.dummy_pos, Lexing.dummy_pos; }

let make : Lexing.position * Lexing.position -> l1expr -> ast_node
= fun info expr -> { expr; info; }

let unit_e = UnitE
let int_e n = IntE n
let bool_e b = BoolE b
let id_e x = Id x

let let_typed id l1type body next 
= Let (id, Some l1type, fresh_node body, fresh_node next)

let let_untyped id body next
= Let (id, None, body, next)

module TEnv = struct 
  type t_env = {
    vars : l1type str_map;
    type_binds : str_set;
  } 
  
  let empty = {
    vars = StrMap.empty;
    type_binds = StrSet.empty;
  }
  
  let find_var : string -> t_env -> l1type
  = fun id env -> StrMap.find id env.vars
  
  let add_var : string -> l1type -> t_env -> t_env
  = fun id l1type env -> { env with vars = StrMap.add id l1type env.vars }
end

type t_env = TEnv.t_env

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
    | FunE (body, arg_name, arg_type, ret_type_option) ->
      begin
        let new_t_env = t_env |> TEnv.add_var arg_name arg_type in
        let body_type = pret body new_t_env in
        match ret_type_option with
        | Some ret_expect ->
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
    | Let (var_name, var_type_option, var_def, next) -> 
      let var_type = pret var_def t_env in
      let var_type = begin
        match var_type_option with
        | Some var_type_expected -> 
          let () = var_type <:! var_type_expected in
          var_type_expected
        | None -> var_type
      end in
      pret next (t_env |> TEnv.add_var var_name var_type)
    | LetRec (fn_name, body, arg_name, arg_type, ret_type, next) -> 
      let new_t_env = t_env |> TEnv.add_var arg_name arg_type in
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
    
        
        
        