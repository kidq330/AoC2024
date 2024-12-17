open Day01

let pair_lists = Io.parse_input ();;

BatPrintf.printf "Answer to first exercise: %d\n" (Ex1.solve pair_lists);;
BatPrintf.printf "Answer to second exercise: %d\n" (Ex2.solve pair_lists)
