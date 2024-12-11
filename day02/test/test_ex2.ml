open OUnit2

let solve_ex2 = Day02.Ex2.solve_ex2

let bug1_tests =
  let input = [1; 4; -1; 7; 10] in
  [ ("bug1:solve_ex2" >:: fun _ -> [input] |> solve_ex2 |> assert_equal 1)
  ; ( "bug1:check_adjacent2"
    >:: fun _ ->
    input
    |> Day02.Ex2.check_adjacent2 (fun x y ->
           let d = y - x in
           1 <= d && d <= 3 )
    |> assert_equal true ) ]

let qtest_counterexample_1 =
  let input = [0; 4; 4] in
  [ ( "qtest_counterexample_1:solve_ex2"
    >:: fun _ -> [input] |> solve_ex2 |> assert_equal 0 )
  ; ( "qtest_counterexample_1:solve_ex2_brute"
    >:: fun _ -> [input] |> Day02.Ex2.solve_ex2_brute |> assert_equal 0 ) ]

let qtest_counterexample_2 =
  let input = [] in
  [ ( "qtest_counterexample_2:solve_ex2"
    >:: fun _ -> [input] |> solve_ex2 |> assert_equal 1 )
  ; ( "qtest_counterexample_2:solve_ex2_brute"
    >:: fun _ -> [input] |> Day02.Ex2.solve_ex2_brute |> assert_equal 1 ) ]

let qtest_counterexample_3 =
  let input = [5; 0; 1] in
  [ ( "qtest_counterexample_3:solve_ex2"
    >:: fun _ -> [input] |> solve_ex2 |> assert_equal 1 )
  ; ( "qtest_counterexample_3:solve_ex2_brute"
    >:: fun _ -> [input] |> Day02.Ex2.solve_ex2_brute |> assert_equal 1 )
  ; ( "qtest_counterexample_3:is_bounded_monotonic2"
    >:: fun _ -> input |> Day02.Ex2.is_bounded_monotonic2 |> assert_equal true
    )
  ; ( "qtest_counterexample_3:is_bounded_increasing"
    >:: fun _ ->
    input |> Day02.Ex2.is_bounded_increasing 1 3 |> assert_equal true )
    (* ; ( "qtest_counterexample_3:check_adjacent2"
    >:: fun _ ->
    input
    |> Day02.Ex2.check_adjacent2 (fun x y -> x == 0 && y == 1)
    |> assert_equal true )  *)
  ]

let qtest_counterexample_4 =
  let input = [0; 2; 1; 2] in
  [ ( "qtest_counterexample_4:solve_ex2"
    >:: fun _ -> [input] |> solve_ex2 |> assert_equal 1 )
  ; ( "qtest_counterexample_4:solve_ex2_brute"
    >:: fun _ -> [input] |> Day02.Ex2.solve_ex2_brute |> assert_equal 1 )
  ; ( "qtest_counterexample_4:is_bounded_increasing"
    >:: fun _ ->
    input |> Day02.Ex2.is_bounded_increasing 1 3 |> assert_equal true )
  ; ( "qtest_counterexample_4:check_adjacent2"
    >:: fun _ ->
    input
    |> Day02.Ex2.check_adjacent2 (fun x y -> y - x == 1)
    |> assert_equal true )
  ; ( "qtest_counterexample_4:check_adjacent2 again"
    >:: fun _ ->
    input
    |> Day02.Ex2.check_adjacent2 (fun x y ->
           let d = y - x in
           1 <= d && d <= 3 )
    |> assert_equal true ) ]

let diff_tests =
  [ ( "diff_tests:list 1"
    >:: fun _ ->
    [70; 68; 71; 73; 76; 77; 78; 80]
    |> BatList.singleton |> solve_ex2 |> assert_equal 1 )
  ; ( "diff_tests:list 1"
    >:: fun _ ->
    [67; 72; 70; 71; 74; 75; 77; 80]
    |> BatList.singleton |> solve_ex2 |> assert_equal 1 )
  ; ( "diff_tests:list 1"
    >:: fun _ ->
    [41; 38; 35; 34; 32; 31; 28; 29]
    |> BatList.singleton |> solve_ex2 |> assert_equal 1 )
  ; ( "diff_tests:list 1"
    >:: fun _ ->
    [82; 85; 84; 87; 90] |> BatList.singleton |> solve_ex2 |> assert_equal 1 )
  ]

let given_tests =
  [ ( "list 1"
    >:: fun _ ->
    [7; 6; 4; 2; 1] |> BatList.singleton |> solve_ex2 |> assert_equal 1 )
  ; ( "list 2"
    >:: fun _ ->
    [1; 2; 7; 8; 9] |> BatList.singleton |> solve_ex2 |> assert_equal 0 )
  ; ( "list 3"
    >:: fun _ ->
    [9; 7; 6; 2; 1] |> BatList.singleton |> solve_ex2 |> assert_equal 0 )
  ; ( "list 4"
    >:: fun _ ->
    [1; 3; 2; 4; 5] |> BatList.singleton |> solve_ex2 |> assert_equal 1 )
  ; ( "list 5"
    >:: fun _ ->
    [8; 6; 4; 4; 1] |> BatList.singleton |> solve_ex2 |> assert_equal 1 )
  ; ( "list 6"
    >:: fun _ ->
    [1; 3; 6; 7; 9] |> BatList.singleton |> solve_ex2 |> assert_equal 1 ) ]

let ounit_tests =
  List.concat
    [ given_tests
    ; diff_tests
    ; bug1_tests
    ; qtest_counterexample_1
    ; qtest_counterexample_2
    ; qtest_counterexample_3
    ; qtest_counterexample_4 ]

let qcheck_tests =
  [ QCheck.Test.make ~count:30
      ~name:"compare brute force solution with the usual"
      QCheck.(list (list small_int))
      (fun input -> solve_ex2 input == Day02.Ex2.solve_ex2_brute input) ]

let _ = Runner.run_tests __MODULE__ ounit_tests qcheck_tests
