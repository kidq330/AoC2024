(*
  This executable was used to find test cases in the input file
  that passed the check performed by the solution to exercise 2,
  but did not pass the harder check from exercise 1.
*)

open Day02;;

let print_list = BatList.print (BatList.print BatInt.print) BatIO.stdout in
() |> Io.parse_input
|> List.filter (fun xs ->
       Ex2.is_bounded_monotonic2 xs && not (Ex1.is_bounded_monotonic xs))
|> print_list
