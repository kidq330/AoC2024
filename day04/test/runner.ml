let run_tests suite_name ounit_tests qcheck_tests = 
  let open OUnit2 in
  suite_name >::: List.concat [ounit_tests; QCheck_ounit.to_ounit2_test_list qcheck_tests] |> run_test_tt_main