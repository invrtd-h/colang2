open OUnit2
open TestSuite

let tests = "test suite for the library fe" >::: [
  "test_let_parsing" >:: (fun _ ->
    let _ = Fe.Run.parse Let.test in
    ()
  );
]

let () = run_test_tt_main tests