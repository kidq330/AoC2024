let parse_input () =
  let line_of_two_ints_re = Str.regexp {|^\(-?[0-9]+\) +\(-?[0-9]+\)|} in
  let parse_pair line =
    if Str.string_match line_of_two_ints_re line 0 then
      let n0 = line |> Str.matched_group 0 |> int_of_string in
      let n1 = line |> Str.matched_group 1 |> int_of_string in
      Some (n0, n1)
    else None
  in
  BatIO.stdin |> BatIO.read_all
  |> Str.split (Str.regexp "\n")
  |> List.filter_map parse_pair |> List.split
