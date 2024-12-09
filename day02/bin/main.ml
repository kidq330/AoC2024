let pair_lists = () |> Day02.read_all_stdin |> Day02.parse_lines_of_ints;;

print_string "Answer to first exercise: ";;
pair_lists |> Day02.solve_ex1 |> print_int;;
print_string "\n";;

print_string "Answer to second exercise: ";;
pair_lists |> Day02.solve_ex2 |> print_int;;
print_string "\n";;
