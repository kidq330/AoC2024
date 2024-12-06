let parse_lines_with_two_integers input =
  let re = Str.regexp {|^\(-?[0-9]+\) +\(-?[0-9]+\)|} in

  let lines = Str.split (Str.regexp "\n") input in

  let extract_ints line =
    if Str.string_match re line 0 then
      let num1 = int_of_string (Str.matched_group 1 line) in
      let num2 = int_of_string (Str.matched_group 2 line) in
      Some (num1, num2)
    else None
  in

  List.filter_map extract_ints lines

let read_all_stdin () =
  let buffer = Buffer.create 4096 in
  try
    while true do
      Buffer.add_char buffer (input_char stdin)
    done;
    Buffer.contents buffer (* UNREACHABLE *)
  with End_of_file -> Buffer.contents buffer

let solve_ex1 (list1, list2) =
  let sort = List.sort compare in
  let list1_sorted = sort list1 in
  let list2_sorted = sort list2 in

  let sum =
    List.fold_left2
      (fun acc a b -> acc + abs (a - b))
      0 list1_sorted list2_sorted
  in

  sum

let solve_ex2 (list1, list2) =
  let module IntMap = Map.Make (Int) in
  let opt_plus_one = function Some n -> Some (n + 1) | None -> Some 1 in
  let map_increment map key = IntMap.update key opt_plus_one map in

  let fq1 = List.fold_left map_increment IntMap.empty list1 in
  let fq2 = List.fold_left map_increment IntMap.empty list2 in

  let score =
    IntMap.fold
      (fun key occurences1 acc ->
        let opt_or_0 a = Option.value a ~default:0 in
        let occurences2 = IntMap.find_opt key fq2 |> opt_or_0 in
        acc + (key * occurences1 * occurences2))
      fq1 0
  in
  score
