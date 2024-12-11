let is_monotonic2 adjacent_f low high xs =
  match xs with
  | [] ->
      true
  | _ :: rest ->
      Utils.is_monotonic_with_skip adjacent_f low high xs
      || Utils.is_monotonic adjacent_f low high rest

let is_bounded_increasing = is_monotonic2 (fun x y -> y - x)

let is_bounded_decreasing = is_monotonic2 ( - )

let is_bounded_monotonic2 xs =
  is_bounded_increasing 1 3 xs || is_bounded_decreasing 1 3 xs

let solve_ex2 matrix =
  matrix |> List.filter is_bounded_monotonic2 |> List.length

let solve_ex2_brute matrix =
  matrix
  |> List.filter (fun xs ->
         match xs with
         | [] ->
             true
         | _ ->
             let all_possibilities_tested_using_ex1 =
               BatList.init (List.length xs) (fun idx ->
                   let xs_without_one = BatList.remove_at idx xs in
                   Ex1.is_bounded_monotonic xs_without_one )
             in
             List.fold_right ( || ) all_possibilities_tested_using_ex1 false )
  |> List.length
