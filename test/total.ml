open OUnit2
open TestSuite

let tests = "test suite for total language features" >::: [
  "int.mte" >:: (fun _ ->
    let v = L1lang.compile_and_run (Fe.Run.parse Int.test) in
    let () = assert_equal v (Expr.IntV 11) in
    ()
  );
  "int_ops.mte" >:: (fun _ ->
    let v = L1lang.compile_and_run (Fe.Run.parse Int_ops.test) in
    let () = assert_equal v (Expr.IntV 121) in
    ()
  );
  "tuple_let.mte" >:: (fun _ ->
    let v = L1lang.compile_and_run (Fe.Parsing.parse Tuple_let.test) in
    let () = assert_equal v (Expr.IntV 121) in
    ()
  );
]

let () = run_test_tt_main tests