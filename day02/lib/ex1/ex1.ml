let is_bounded_increasing = Utils.is_monotonic (fun x y -> y - x)

let is_bounded_decreasing = Utils.is_monotonic ( - )

let is_bounded_monotonic xs =
  is_bounded_increasing 1 3 xs || is_bounded_decreasing 1 3 xs

let solve_ex1 matrix = matrix |> List.filter is_bounded_monotonic |> List.length
