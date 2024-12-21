open Day04

let input = Io.parse_input ();;

(* WIP: Owl is not available for arm64 and I couldn't be bothered to look for other numpy stand-ins*)
BatPrintf.printf "Answer to first exercise: %d\n" (Ex1.solve input)
