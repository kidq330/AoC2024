open Utils

let parse_input () = BatIO.stdin |> BatIO.read_all |> CharMatrix.of_string
