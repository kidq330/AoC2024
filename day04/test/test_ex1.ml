open OUnit2
open Day04.Private

let solve = Ex1.solve

let generate_seq_ounit =
  [
    ( "generate_seq:cluttered" >:: fun _ ->
      let str = ".XMAS." in
      Ex1.generate_seq (1, 0) (1, 0) (fun (x, y) ->
          match y with
          | 0 -> String.get str x
          | _ ->
              y
              |> BatPrintf.sprintf "y coordinate unexpectedly non-zero: y=%d"
              |> OUnit2.assert_failure)
      |> String.of_seq
      |> assert_equal "XMAS." ~printer:Fun.id );
    ( "generate_seq:starts_with" >:: fun _ ->
      String.starts_with ~prefix:"XMAS" "XMAS." |> assert_equal true );
    ( "generate_seq:with matrix" >:: fun _ ->
      let m = ".XMAS." |> Day04.CharMatrix.of_string in
      Ex1.generate_seq (1, 0) (1, 0) (fun (x, y) ->
          match y with
          | 0 -> m.(0).(x)
          | _ ->
              y
              |> BatPrintf.sprintf "y coordinate unexpectedly non-zero: y=%d"
              |> OUnit2.assert_failure)
      |> String.of_seq
      |> assert_equal "XMAS." ~printer:Fun.id );
  ]

let dhp_ounit =
  [
    ( "dhp:cluttered" >:: fun _ ->
      let m = ".XMAS." |> Day04.CharMatrix.of_string in
      Ex1.direction_has_pattern "XMAS" m (1, 0) (1, 0) |> assert_equal true );
    ( "dhp:simpler" >:: fun _ ->
      let m = ".XMAS." |> Day04.CharMatrix.of_string in
      Ex1.direction_has_pattern "X" m (1, 0) (1, 0) |> assert_equal true );
  ]

let cdp_ounit =
  [
    ( "cdp:simple" >:: fun _ ->
      let m = "XMAS" |> Day04.CharMatrix.of_string in
      (0, 0)
      |> Ex1.count_directed_patterns "XMAS" m
      |> assert_equal 1 ~printer:Int.to_string );
    ( "cdp:simple, miss" >:: fun _ ->
      let m = "XMAS" |> Day04.CharMatrix.of_string in
      (1, 0)
      |> Ex1.count_directed_patterns "XMAS" m
      |> assert_equal 0 ~printer:Int.to_string );
    ( "cdp:cluttered" >:: fun _ ->
      let m = ".XMAS." |> Day04.CharMatrix.of_string in
      (1, 0)
      |> Ex1.count_directed_patterns "XMAS" m
      |> assert_equal 1 ~printer:Int.to_string );
    ( "cdp:reversed" >:: fun _ ->
      let m = ".SAMX." |> Day04.CharMatrix.of_string in
      (4, 0)
      |> Ex1.count_directed_patterns "XMAS" m
      |> assert_equal 1 ~printer:Int.to_string );
  ]

let solve_ounit =
  [
    ( "solve:simple" >:: fun _ ->
      "XMAS" |> Day04.CharMatrix.of_string |> solve
      |> assert_equal 1 ~printer:Int.to_string );
    ( "solve:reversed" >:: fun _ ->
      "SAMX" |> Day04.CharMatrix.of_string |> solve
      |> assert_equal 1 ~printer:Int.to_string );
    ( "solve:vertical" >:: fun _ ->
      "X\nM\nA\nS" |> Day04.CharMatrix.of_string |> solve
      |> assert_equal 1 ~printer:Int.to_string );
    ( "solve:vertical, reversed" >:: fun _ ->
      "S\nA\nM\nX" |> Day04.CharMatrix.of_string |> solve
      |> assert_equal 1 ~printer:Int.to_string );
    ( "solve:two rows" >:: fun _ ->
      "XMAS\nSAMX" |> Day04.CharMatrix.of_string |> solve
      |> assert_equal 2 ~printer:Int.to_string );
    ( "solve:cluttered" >:: fun _ ->
      ".XMAS." |> Day04.CharMatrix.of_string |> solve
      |> assert_equal 1 ~printer:Int.to_string );
    ( "solve:two rows, multiline string" >:: fun _ ->
      {|XMAS
SAMX|} |> Day04.CharMatrix.of_string |> solve
      |> assert_equal 2 ~printer:Int.to_string );
    (* ("solve:given with dots" >:: fun _ ->
      {|....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX|}
      |> Day04.CharMatrix.of_string |> solve |> assert_equal 1 ~printer:Int.to_string8 );
    ( "solve:given" >:: fun _ ->
      {|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|}
      |> Day04.CharMatrix.of_string |> solve |> assert_equal 1 ~printer:Int.to_string8 ); *)
  ]

let ounit_tests =
  List.concat [ generate_seq_ounit; dhp_ounit; cdp_ounit; solve_ounit ]

let qcheck_tests = []
let _ = Runner.run_tests __MODULE__ ounit_tests qcheck_tests
