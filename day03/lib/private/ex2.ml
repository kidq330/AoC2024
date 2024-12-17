let re_cond = Re.(alt [ str "do()"; str "don't()"; Ex1.re_mul ])

type solve_accumulator_t = { include_muls : bool; sum : int }

let solve input =
  let rc = Re.compile re_cond in
  let acc_f acc gmatch =
    match Re.Group.get gmatch 0 with
    | "do()" -> { acc with include_muls = true }
    | "don't()" -> { acc with include_muls = false }
    | mul when String.starts_with ~prefix:"mul(" mul ->
        let { include_muls; sum } = acc in
        if include_muls then
          let arg1, arg2 = Ex1.extract_args gmatch in
          { acc with sum = sum + (arg1 * arg2) }
        else acc
    | otherwise ->
        otherwise
        |> BatPrintf.sprintf "Matching failed on string %s"
        |> failwith
  in
  let { sum; _ } =
    input |> Re.all rc |> List.fold_left acc_f { include_muls = true; sum = 0 }
  in
  sum
