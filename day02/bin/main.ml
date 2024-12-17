open Day02

let levels = Io.parse_input ();;

(* regression: 411 *)
BatPrintf.printf "Answer to first exercise: %d\n" (Ex1.solve levels);;

(* regression: 465 *)
BatPrintf.printf "Answer to second exercise: %d\n" (Ex2.solve levels)
