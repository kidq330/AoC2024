
let read_all_stdin () =
  let buffer = Buffer.create 4096 in
  try
    while true do
      Buffer.add_char buffer (input_char stdin)
    done;
    Buffer.contents buffer (* UNREACHABLE *)
  with End_of_file -> Buffer.contents buffer
  
let parse_string_of_ints str_ints = 
  str_ints
  |> Str.split (Str.regexp {| |})
  |> List.map int_of_string

let parse_lines_of_ints input =
  input 
  |> Str.split (Str.regexp "\n")
  |> List.map parse_string_of_ints
  

let rec check_adjacent pred xs = match xs with
  | x :: y :: xs ->
    (pred x y) && (check_adjacent pred (y :: xs))
  | _ -> true

let is_bounded_increasing low_diff high_diff = check_adjacent (fun x y -> 
  let diff = y - x in
  low_diff <= diff && diff <= high_diff)

let is_bounded_decreasing low_diff high_diff = check_adjacent (fun x y -> 
  let diff = x - y in
  low_diff <= diff && diff <= high_diff)

let solve_ex1 matrix =
  let check_monotonic xs =
    is_bounded_decreasing 1 3 xs ||
    is_bounded_increasing 1 3 xs
  in
  matrix
  |> List.filter check_monotonic
  |> List.length

(* let solve_ex2 (list1, list2) = () *)