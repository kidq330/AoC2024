open OUnit2

let solve_ex1 = Day02.Ex1.solve_ex1

let given_tests =
  [ ( "list 1"
    >:: fun _ ->
    [7; 6; 4; 2; 1] |> BatList.singleton |> solve_ex1 |> assert_equal 1 )
  ; ( "list 2"
    >:: fun _ ->
    [1; 2; 7; 8; 9] |> BatList.singleton |> solve_ex1 |> assert_equal 0 )
  ; ( "list 3"
    >:: fun _ ->
    [9; 7; 6; 2; 1] |> BatList.singleton |> solve_ex1 |> assert_equal 0 )
  ; ( "list 4"
    >:: fun _ ->
    [1; 3; 2; 4; 5] |> BatList.singleton |> solve_ex1 |> assert_equal 0 )
  ; ( "list 5"
    >:: fun _ ->
    [8; 6; 4; 4; 1] |> BatList.singleton |> solve_ex1 |> assert_equal 0 )
  ; ( "list 6"
    >:: fun _ ->
    [1; 3; 6; 7; 9] |> BatList.singleton |> solve_ex1 |> assert_equal 1 ) ]

let qtest_counterexample_1 =
  let input = [0; 4; 4] in
  let open Day02.Ex1 in
  [ ( "qtest_counterexample_1:solve_ex1"
    >:: fun _ -> input |> is_bounded_monotonic |> assert_equal false )
  ; ( "qtest_counterexample_1:solve_ex1 remove_at 0"
    >:: fun _ ->
    BatList.remove_at 0 input |> is_bounded_monotonic |> assert_equal false )
  ; ( "qtest_counterexample_1:solve_ex1 remove_at 1"
    >:: fun _ ->
    BatList.remove_at 1 input |> is_bounded_monotonic |> assert_equal false )
  ; ( "qtest_counterexample_1:solve_ex1 remove_at 2"
    >:: fun _ ->
    BatList.remove_at 2 input |> is_bounded_monotonic |> assert_equal false )
  ; ( "qtest_counterexample_1:solve_ex1 BatList.init test"
    >:: fun _ ->
    assert_equal
      [[4; 4]; [0; 4]; [0; 4]]
      (BatList.init (List.length input) (fun idx -> BatList.remove_at idx input))
    )
  ; ( "qtest_counterexample_1:solve_ex1 BatList.init test with \
       is_bounded_monotonic"
    >:: fun _ ->
    assert_equal [false; false; false]
      (BatList.init (List.length input) (fun idx ->
           let xs_without_one = BatList.remove_at idx input in
           is_bounded_monotonic xs_without_one ) ) )
  ; ( "qtest_counterexample_1:solve_ex1 List.fold_right check"
    >:: fun _ ->
    assert_equal false (List.fold_right ( || ) [false; false; false] false) )
  ; ( "qtest_counterexample_1:solve_ex1 List.fold_left check"
    >:: fun _ ->
    assert_equal false (List.fold_left ( || ) false [false; false; false]) ) ]

let ounit_tests = List.concat [given_tests; qtest_counterexample_1]

let qcheck_tests = []

let _ = Runner.run_tests __MODULE__ ounit_tests qcheck_tests
