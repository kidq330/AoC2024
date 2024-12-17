open OUnit2

let bug1_tests =
  let input = [1; 4; -1; 7; 10] in
  [ ( "bug1:check_adjacent_one_skip"
    >:: fun _ ->
    input
    |> Day02.Utils.check_adjacent_one_skip (fun x y ->
           let d = y - x in
           1 <= d && d <= 3 )
    |> assert_equal true ) ]

let ounit_tests = List.concat [bug1_tests]

let qcheck_tests = []

let _ = Runner.run_tests __MODULE__ ounit_tests qcheck_tests
