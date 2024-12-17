type 'elem_t list_traversing_predicate =
  ('elem_t -> 'elem_t -> bool) -> 'elem_t list -> bool

type ('elem_t, 'b) bounded_mono_checker =
  ('elem_t -> 'elem_t -> 'b) -> 'b -> 'b -> 'elem_t list -> bool

let rec check_adjacent (pred : 'elem_t -> 'elem_t -> bool) (xs : 'elem_t list) =
  match xs with
  | x :: y :: rest ->
      pred x y && check_adjacent pred (y :: rest)
  | _ ->
      true

let rec check_adjacent_one_skip pred xs =
  match xs with
  | x :: y :: rest ->
      let noskip_result =
        if pred x y then check_adjacent_one_skip pred (y :: rest) else false
      in
      noskip_result || check_adjacent pred (x :: rest)
  | _ ->
      true

let _is_monotonic_check_template (fold_f : 'elem_t list_traversing_predicate)
    (adjacent_f : 'elem_t -> 'elem_t -> 'b) (low : 'b) (high : 'b) =
  fold_f (fun x y ->
      let adj = adjacent_f x y in
      low <= adj && adj <= high )

let is_monotonic adj_f low high =
  _is_monotonic_check_template check_adjacent adj_f low high

let is_monotonic_with_skip adj_f low high =
  _is_monotonic_check_template check_adjacent_one_skip adj_f low high
