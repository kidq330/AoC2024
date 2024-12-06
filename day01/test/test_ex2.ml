open OUnit2
let solve_ex2 = Day01.solve_ex2;;

let ounit_tests = [
  "keymashed inputs" >:: (fun _ -> 
    let list1 = [39248; 4389; 320] in
    let list2 = [4390; 39247; 319] in
    let input = (list1, list2) in
    assert_equal 0 (solve_ex2 input));
  "no duplicates" >:: (fun _ -> 
    let list1 = [1; 2; 3] in
    let list2 = [3; 1; 2; 4] in
    let input = (list1, list2) in
    assert_equal 
      (solve_ex2 input)
      (1 + 2 + 3)
      );
  "handcrafted inputs" >:: (fun _ -> 
    let list1 = [1; 2; 2; 3; 3; 3; 4] in
    let list2 = [4; 4; 3; 2; 2; 2; 1; 1; 1; 1] in
    let input = (list1, list2) in
    assert_equal 
      (solve_ex2 input)
      ( 1 * 1 * 4
      + 2 * 2 * 3
      + 3 * 3 * 1
      + 4 * 1 * 2)
      );
]

let qcheck_tests = []

let _ = Runner.run_tests __MODULE__ ounit_tests qcheck_tests
