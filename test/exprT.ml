open OUnit2
open Expr

(* let id = e1 in e2 *)
let let_e id e1 e2 = Apply (FunE (e2, id), e1)

let rec join expr_list = match expr_list with
| h :: [] -> h
| h :: t -> Seqn (h, join t)
| [] -> UnitE

let vtos value = match value with
| UnitV -> "UnitV"
| IntV n -> Printf.sprintf "Int(%d)" n
| FunV (_, _, _) -> "FunV"

let tests = "test suite for mem.ml" >::: [
  "test_add" >:: (fun _ ->
    let e = add_e (IntE 2) (IntE 3) in
    let _ = assert_equal (run e) (IntV 5) in
    ()
  );
  "test_fun" >:: (fun _ ->
    let e = let_e 6974 (IntE 1) (add_e (Id 6974) (Id 6974)) in
    let _ = assert_equal (run e) (IntV 2) in
    () 
  );
  "test_unit" >:: (fun _ -> assert_equal UnitV (run UnitE));
  "test_join" >:: (fun _ ->
    let l = [IntE 1; IntE 2; IntE 3; IntE 4;] in
    let e = Seqn (IntE 1, Seqn (IntE 2, Seqn (IntE 3, IntE 4))) in
    assert_equal e (join l)
  );
  "test_assign_in_function" >:: (fun _ ->
    let e1_frag = let_e 6974 (IntE 1) in
    let e2_frag = let_e 6975 (FunE (Assign (6974, Id 6000), 6000)) in
    let e3_frag = let_e 6976 (Apply (Id 6975, IntE 4)) in
    let e = e1_frag @@ e2_frag @@ e3_frag @@ (add_e (Id 6974) (Id 6974)) in
    let _ = assert_equal (IntV 8) (run e) ~printer:vtos in
    ()
  ); 
]

let _ = run_test_tt_main tests