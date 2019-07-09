let () =
  let argv_list = Array.to_list Sys.argv in
  match argv_list with
  | [a] ->
    stdin |> Reader.read |> Eval.eval
  | [a; b] ->
    b |> open_in |> Reader.read |> Eval.eval
  | _ -> Error._failwith "Invalid number of arguments"
