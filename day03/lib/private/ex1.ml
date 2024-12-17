let re_int = Re.(repn digit 1 (Some 3))

let re_mul =
  Re.(seq [ str "mul("; group re_int; char ','; group re_int; char ')' ])

let extract_args group_match =
  let arg1 = Re.Group.get group_match 1 |> int_of_string in
  let arg2 = Re.Group.get group_match 2 |> int_of_string in
  (arg1, arg2)

let solve str =
  let rc = Re.compile re_mul in
  let acc_f =
   fun acc gmatch ->
    let arg1, arg2 = extract_args gmatch in
    acc + (arg1 * arg2)
  in
  str |> Re.all rc |> List.fold_left acc_f 0
