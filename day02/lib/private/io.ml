let parse_input () =
  BatIO.stdin |> BatIO.read_all |> String.split_on_char '\n'
  |> List.map (fun line ->
         line |> String.split_on_char ' ' |> List.map int_of_string)
