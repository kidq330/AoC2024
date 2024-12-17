let solve (list1, list2) =
  let module IntMap = Map.Make (Int) in
  let opt_plus_one = function Some n -> Some (n + 1) | None -> Some 1 in
  let map_increment map key = IntMap.update key opt_plus_one map in

  let fq1 = List.fold_left map_increment IntMap.empty list1 in
  let fq2 = List.fold_left map_increment IntMap.empty list2 in

  let score =
    IntMap.fold
      (fun key occurences1 acc ->
        let opt_or_0 a = Option.value a ~default:0 in
        let occurences2 = IntMap.find_opt key fq2 |> opt_or_0 in
        acc + (key * occurences1 * occurences2))
      fq1 0
  in
  score
