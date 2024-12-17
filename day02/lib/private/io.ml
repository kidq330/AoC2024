let parse_input () =
  BatIO.stdin |> BatIO.read_all
  |> Str.split (Str.regexp "\n")
  |> List.map (fun line ->
         line |> Str.split (Str.regexp " ") |> List.map int_of_string )
