let read_all_stdin () =
  let buffer = Buffer.create 4096 in
  try
    while true do
      Buffer.add_char buffer (input_char stdin)
    done ;
    Buffer.contents buffer (* UNREACHABLE *)
  with End_of_file -> Buffer.contents buffer

let parse_string_of_ints str_ints =
  str_ints |> Str.split (Str.regexp " ") |> List.map int_of_string

let parse_lines_of_ints input =
  input |> Str.split (Str.regexp "\n") |> List.map parse_string_of_ints
