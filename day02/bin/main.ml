let pair_lists = () |> Day02.Io.read_all_stdin |> Day02.Io.parse_lines_of_ints
;;

(* regression: 411 *)
print_string "Answer to first exercise: " ;;

pair_lists |> Day02.Ex1.solve_ex1 |> print_int ;;

print_string "\n" ;;

(* regression: 468 *)
print_string "Answer to second exercise: " ;;

pair_lists |> Day02.Ex2.solve_ex2 |> print_int ;;

print_string "\n"
