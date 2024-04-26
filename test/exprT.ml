open OUnit2
open Lang.Expr

let [@warning "-unused-value-declaration"] true_e 
= Value (BoolV true)
let [@warning "-unused-value-declaration"] false_e 
= Value (BoolV false)

(* e1; e2 *)
let seqn e1 e2 = Help.let' 0 e1 e2

let [@warning "-unused-value-declaration"] rec
join expr_list = match expr_list with
| [] -> Help.unit
| h :: [] -> h
| h :: t -> seqn h (join t)

let ( ++ ) l r = Help.add l r
let ( -- ) l r = Help.sub l r 
let ( *** ) l r = Help.mul l r
let ( !$ ) n = Id n
let ( !* ) n = Help.int n
  
let vtos value = match value with
| UnitV -> "UnitV"
| IntV n -> Printf.sprintf "%d_v" n
| BoolV b -> Format.sprintf "%b_v" b
| FunV (_, _, _) -> "FunV"
| VecV _ -> "VecV"
| TupleV _ -> "TupleV"

let tests = "test suite for expr.ml" >::: [
  "test_add" >:: (fun _ ->
    let e = Help.add (Help.int 2) (Help.int 3) in
    let _ = assert_equal (run e) (IntV 5) in
    ()
  );
  "test_fun" >:: (fun _ ->
    let e = Help.let' 6974 (Help.int 1) (Help.add !$6974 !$6974) in
    let _ = assert_equal (run e) (IntV 2) in
    () 
  );
  "test_unit" >:: (fun _ -> assert_equal UnitV (run Help.unit));
  "test_assign_in_function" >:: (fun _ ->
    let e1_frag = Help.let' 6974 (Help.int 1) in
    let e2_frag = Help.let' 6975 (FunE (Assign (6974, Id 6000), 6000)) in
    let e3_frag = Help.let' 6976 (Apply (Id 6975, Help.int 4)) in
    let e = e1_frag @@ e2_frag @@ e3_frag @@ (Help.add (Id 6974) (Id 6974)) in
    let _ = assert_equal (IntV 8) (run e) ~printer:vtos in
    ()
  );
  "test_seqn" >:: (fun _ ->
    let e = Help.let' 6974 (Help.int 1) (seqn (Assign (6974, Help.int 7)) (Id 6974)) in
    let _ = assert_equal (IntV 7) (run e) ~printer:vtos in
    ()
  );
  "test_let_join" >:: (fun _ ->
    let e = Help.let_join [
      Help.let' 6974 !*1;
      Help.let' 6975 (!$6974 ++ !$6974);
      Help.let' 6976 (!$6975 ++ !$6975);
      Help.let' 6977 (!$6976 ++ !$6976);
    ] (Id 6977) in
    let _ = assert_equal (IntV 8) (run e) ~printer:vtos in
    ()
  );
  "test_rec" >:: (fun _ ->
    let e = Help.let_join [
      Help.let_rec 111 (Help.if' (Help.lt !$1 !*1) !*1 (!$1 *** Help.apply !$111 (!$1 -- !*1))) 1;
    ] (Help.apply !$111 !*5) in
    let _ = assert_equal (IntV 120) (run e) ~printer:vtos in
    ()
  );
  "test_if_no_eval" >:: (fun _ ->
    let e = Help.let_join [
      Help.let' 6974 !*0;
      Help.let' 6975 (Help.if' (Help.lt !*1 !*2) Help.unit (Assign (6974, !*1)));
    ] !$6974 in
    let _ = assert_equal (IntV 0) (run e) ~printer:vtos in
    ()
  );
  "test_memory" >:: (fun _ ->
    let e = Help.let_join [
      Help.let' 6974 !*1;
      Help.let' 6975 (!$6974 ++ !$6974);
      Help.let' 6976 (!$6975 ++ !$6975);
      Help.let' 6977 (!$6976 ++ !$6976);
    ] (Id 6977) in
    let env = new_env in
    let mem = Lang.Mem.create () in
    let _ = assert_equal (IntV 8) (pret e env mem) ~printer:vtos in
    let _ = assert_equal 0 (Lang.Mem.length mem) in
    let _ = assert_equal 4 (Lang.Mem.Vec.length mem.data) in
    ()
  )
]

let () = run_test_tt_main tests