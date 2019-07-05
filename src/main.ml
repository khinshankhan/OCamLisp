let () =
  let argv_list = Array.to_list Sys.argv in
  match argv_list with
  | [a] ->
    stdin |> Reader.parse
  | [a; b] ->
    b|> open_in |> Reader.parse
  | _ -> failwith "Invalid number of arguments"
