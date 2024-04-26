open OUnit2
open Lang.L1lang
open Lang.L1type

let let_single_typed_e id type_expr body next = 
  Let (VarIdLP (id, Some type_expr), body, next)

let tests = "test suite for expr.ml" >::: [
  "test_add" >:: (
    fun _ -> 
      let e = let_single_typed_e "x" TE.int (E.int 3) E.unit in
      let () = type_check e in
      ()
  );
]

let () = run_test_tt_main tests