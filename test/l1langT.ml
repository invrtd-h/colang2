open OUnit2
open L1lang

let tests = "test suite for expr.ml" >::: [
  "test_add" >:: (
    fun _ -> 
      let e = let_typed "x" bool_t (int_e 3) unit_e in
      let report = e |> fresh |> type_check in
      let () = print_endline (List.hd report) in
      ()
  )
]

let () = run_test_tt_main tests