open OUnit2

let tests = "test suite for mem.ml" >::: [
  "test1" >:: (fun _ -> 
    let m = Mem.create () in
    let _ = Mem.push m 1 in
    let _ = Mem.push m 5 in
    let _ = Mem.push m 9 in
    let _ = assert_equal 1 (Mem.get m 0) in
    let _ = Mem.kill m 0 in 
    let _ = assert_equal 0 (Mem.malloc m) in
    ()
  );
  "test2" >:: (fun _ ->
    let m = Mem.create () in
    let _ = for i = 0 to 19 do 
      Mem.push m i
    done in
    let _ = for i = 1 to 3 do 
      Mem.kill m (i * 5)
    done in
    let _ = assert_equal 5 (Mem.malloc m) in
    let _ = assert_equal 10 (Mem.malloc m) in
    let _ = assert_equal 15 (Mem.malloc m) in
    let _ = assert_equal 20 (Mem.malloc m) in
    let _ = assert_equal 21 (Mem.malloc m) in
    ()
  )
]

let _ = run_test_tt_main tests