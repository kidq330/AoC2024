let pair_lists = () |> Day01.read_all_stdin |> Day01.parse_lines_with_two_integers |> List.split;;

print_string "Answer to first exercise: ";;
pair_lists |> Day01.solve_ex1 |> print_int;;
print_string "\n";;

print_string "Answer to second exercise: ";;
pair_lists |> Day01.solve_ex2 |> print_int;;
print_string "\n";;
  