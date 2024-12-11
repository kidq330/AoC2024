(*
  This executable was used to find test cases in the input file
  that passed the check performed by the solution to exercise 2,
  but did not pass the harder check from exercise 1.
*)

open Day02 ;;

let print_list xs =
  let () =
    List.iter
      (fun x ->
        let () = x |> print_int in
        print_string " " )
      xs
  in
  print_endline ""
in
() |> Io.read_all_stdin |> Io.parse_lines_of_ints
|> List.filter (fun xs ->
       Ex2.is_bounded_monotonic2 xs && not (Ex1.is_bounded_monotonic xs) )
|> List.iter print_list
