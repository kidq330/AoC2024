let solve (list1, list2) =
  let sort = List.sort compare in
  let list1_sorted = sort list1 in
  let list2_sorted = sort list2 in

  let sum =
    List.fold_left2
      (fun acc a b -> acc + abs (a - b))
      0 list1_sorted list2_sorted
  in

  sum
