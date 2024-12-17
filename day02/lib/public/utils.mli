type 'elem_t list_traversing_predicate =
  ('elem_t -> 'elem_t -> bool) -> 'elem_t list -> bool

val check_adjacent : 'elem_t list_traversing_predicate

val check_adjacent_one_skip : 'elem_t list_traversing_predicate

(* bounded_mono_checker = adjacent_operator -> lower_threshold -> upper_treshold -> list -> is_bounded_monotonic *)
type ('elem_t, 'b) bounded_mono_checker =
  ('elem_t -> 'elem_t -> 'b) -> 'b -> 'b -> 'elem_t list -> bool

val is_monotonic : ('elem_t, 'b) bounded_mono_checker

val is_monotonic_with_skip : ('elem_t, 'b) bounded_mono_checker
