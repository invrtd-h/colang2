open OUnit2
open TestSuite
open Fe.Run2
open Lang

let tests = "test suite for total language features" >::: [
  "int_test.mte" >:: (fun _ ->
    let v = L1lang.compile_and_run (parse Int_test.test.(0)) in
    let () = assert_equal v (Expr.IntV 11) in
    ()
  );
  "int_ops.mte" >:: (fun _ ->
    let v = L1lang.compile_and_run (parse Int_ops.test.(0)) in
    let () = assert_equal v (Expr.IntV 121) in
    ()
  );
  "tuple_let.mte" >:: (fun _ ->
    let v = L1lang.compile_and_run (parse Tuple_let.test.(0)) in
    let () = assert_equal v (Expr.IntV 12221) in
    ()
  );
  "wrongSyntax.mte" >:: (fun _ ->
    try
      let _ = run WrongSyntax.test.(0) in
      failwith "Test Failed: wrongSyntax.mte must fail since it has a wrong syntax"
    with
      | ParserError s -> Printf.printf "%s" s
  );
]

let () = run_test_tt_main tests