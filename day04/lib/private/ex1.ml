open Utils

let generate_seq (x, y) (dx, dy) f =
  let rec next i () =
    try
      let xi = x + (i * dx) in
      let yi = y + (i * dy) in
      let elem = f (xi, yi) in
      Seq.Cons (elem, next (i + 1))
    with _ -> Seq.Nil
  in
  next 0

let direction_has_pattern pattern (m : CharMatrix.t) xy dxy =
  generate_seq xy dxy (fun (x, y) -> m.(y).(x))
  |> String.of_seq
  |> String.starts_with ~prefix:pattern

let count_directed_patterns pattern (m : CharMatrix.t) xy =
  let basis =
    [ (1, 0); (1, -1); (0, -1); (-1, -1); (-1, 0); (-1, 1); (0, 1); (1, 1) ]
  in
  basis |> List.filter (direction_has_pattern pattern m xy) |> List.length

let solve (m : CharMatrix.t) =
  let () = assert (Array.length m > 0) in
  let n1 = Array.length m in
  let n2 = Array.length m.(0) in
  let indices =
    BatList.(cartesian_product (range 0 `To (n2 - 1)) (range 0 `To (n1 - 1)))
  in
  indices
  |> List.filter (fun (x, y) -> m.(y).(x) == 'X')
  |> List.map (count_directed_patterns "XMAS" m)
  |> List.fold_left ( + ) 0
