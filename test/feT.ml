open OUnit2
open TestSuite

let tests = "test suite for the library fe" >::: [
  "test_let_parsing" >:: (fun _ ->
    let _ = Fe.Run.parse Let.test.(0) in
    ()
  );
  "test_type_parsing" >:: (fun _ ->
    let _ = Fe.Run.parse Type.test.(0) in
    ()
  );
]

let () = run_test_tt_main tests