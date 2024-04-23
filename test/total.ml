open OUnit2
open TestSuite
open Fe.Run2

let tests = "test suite for total language features" >::: [
  "int.mte" >:: (fun _ ->
    let v = L1lang.compile_and_run (parse Int.test) in
    let () = assert_equal v (Expr.IntV 11) in
    ()
  );
  "int_ops.mte" >:: (fun _ ->
    let v = L1lang.compile_and_run (parse Int_ops.test) in
    let () = assert_equal v (Expr.IntV 121) in
    ()
  );
  "tuple_let.mte" >:: (fun _ ->
    let v = L1lang.compile_and_run (parse Tuple_let.test) in
    let () = assert_equal v (Expr.IntV 12221) in
    ()
  );
  "wrongSyntax.mte" >:: (fun _ ->
    try
      let _ = run WrongSyntax.test in
      failwith "Test Failed: wrongSyntax.mte must fail since it has a wrong syntax"
    with
      | ParserError s -> Printf.printf "%s" s
  );
]

let () = run_test_tt_main tests