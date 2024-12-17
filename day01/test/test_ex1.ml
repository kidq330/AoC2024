open OUnit2

let solve = Day01.Ex1.solve

let ounit_tests =
  [
    ( "keymashed inputs" >:: fun _ ->
      let list1 = [ 39248; 4389; 320 ] in
      let list2 = [ 4390; 39247; 319 ] in
      let input = (list1, list2) in
      assert_equal 3 (solve input) );
  ]

let qcheck_tests =
  [
    QCheck.Test.make ~count:100 ~name:"identical lists evaluate to 0"
      QCheck.(list small_int)
      (fun input_list ->
        let input = (input_list, input_list) in
        solve input == 0);
  ]

let _ = Runner.run_tests __MODULE__ ounit_tests qcheck_tests
