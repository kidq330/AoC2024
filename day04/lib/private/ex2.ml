let match_2d_pattern_with_wildcards (pattern : ICharMatrix.t)
    (m : ICharMatrix.t) =
  let p = pattern in
  let n1 = Array.length m in
  let p1 = Array.length pattern in
  let () = assert (n1 > 0) in
  let () = assert (p1 > 0) in
  let n2 = Array.length m.(0) in
  let p2 = Array.length p.(0) in
  let indices =
    BatList.(cartesian_product (range 0 `To (n2 - p2)) (range 0 `To (n1 - p1)))
  in
  _

let solve (m : ICharMatrix.t) =
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
