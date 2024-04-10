open OUnit2
open Expr

let [@warning "-unused-value-declaration"] true_e 
= Value (BoolV true)
let [@warning "-unused-value-declaration"] false_e 
= Value (BoolV false)

(* e1; e2 *)
let seqn e1 e2 = let_e 0 e1 e2

let [@warning "-unused-value-declaration"] rec
join expr_list = match expr_list with
| [] -> UnitE
| h :: [] -> h
| h :: t -> seqn h (join t)

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

let ( ++ ) l r = add_e l r
let ( -- ) l r = sub_e l r 
let ( *** ) l r = mul_e l r
let ( !$ ) n = Id n
let ( !* ) n = int_e n
  
let vtos value = match value with
| UnitV -> "UnitV"
| IntV n -> Printf.sprintf "%d_v" n
| BoolV b -> Format.sprintf "%b_v" b
| FunV (_, _, _) -> "FunV"
| VecV _ -> "VecV"
| TupleV _ -> "TupleV"

let tests = "test suite for mem.ml" >::: [
  "test_add" >:: (fun _ ->
    let e = add_e (int_e 2) (int_e 3) in
    let _ = assert_equal (run e) (IntV 5) in
    ()
  );
  "test_fun" >:: (fun _ ->
    let e = let_e 6974 (int_e 1) (add_e !$6974 !$6974) in
    let _ = assert_equal (run e) (IntV 2) in
    () 
  );
  "test_unit" >:: (fun _ -> assert_equal UnitV (run UnitE));
  "test_assign_in_function" >:: (fun _ ->
    let e1_frag = let_e 6974 (int_e 1) in
    let e2_frag = let_e 6975 (FunE (Assign (6974, Id 6000), 6000)) in
    let e3_frag = let_e 6976 (Apply (Id 6975, int_e 4)) in
    let e = e1_frag @@ e2_frag @@ e3_frag @@ (add_e (Id 6974) (Id 6974)) in
    let _ = assert_equal (IntV 8) (run e) ~printer:vtos in
    ()
  );
  "test_seqn" >:: (fun _ ->
    let e = let_e 6974 (int_e 1) (seqn (Assign (6974, int_e 7)) (Id 6974)) in
    let _ = assert_equal (IntV 7) (run e) ~printer:vtos in
    ()
  );
  "test_let_join" >:: (fun _ ->
    let e = let_join [
      let_e 6974 !*1;
      let_e 6975 (!$6974 ++ !$6974);
      let_e 6976 (!$6975 ++ !$6975);
      let_e 6977 (!$6976 ++ !$6976);
    ] (Id 6977) in
    let _ = assert_equal (IntV 8) (run e) ~printer:vtos in
    ()
  );
  "test_rec" >:: (fun _ ->
    let e = let_join [
      let_rec 111 (if_e (lt_e !$1 !*1) !*1 (!$1 *** apply !$111 (!$1 -- !*1))) 1;
    ] (apply !$111 !*5) in
    let _ = assert_equal (IntV 120) (run e) ~printer:vtos in
    ()
  );
  "test_if_no_eval" >:: (fun _ ->
    let e = let_join [
      let_e 6974 !*0;
      let_e 6975 (if_e (lt_e !*1 !*2) UnitE (Assign (6974, !*1)));
    ] !$6974 in
    let _ = assert_equal (IntV 0) (run e) ~printer:vtos in
    ()
  );
  "test_memory" >:: (fun _ ->
    let e = let_join [
      let_e 6974 !*1;
      let_e 6975 (!$6974 ++ !$6974);
      let_e 6976 (!$6975 ++ !$6975);
      let_e 6977 (!$6976 ++ !$6976);
    ] (Id 6977) in
    let env = new_env in
    let mem = Mem.create () in
    let _ = assert_equal (IntV 8) (pret e env mem) ~printer:vtos in
    let _ = assert_equal 0 (Mem.length mem) in
    let _ = assert_equal 4 (Mem.Vec.length mem.data) in
    ()
  )
]

let _ = run_test_tt_main tests