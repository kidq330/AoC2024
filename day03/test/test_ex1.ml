open OUnit2

let solve = Day03.Ex1.solve

let ounit_tests =
  [
    ( "simple test" >:: fun _ ->
      "11mul(22,33)22mul(9,10)andmul(3,4)0ismul(a,3)"
      |> solve |> assert_equal (22 * 33 + 9 * 10 + 3 * 4)
      )
  ]

let qcheck_tests =
  []

let _ = Runner.run_tests __MODULE__ ounit_tests qcheck_tests