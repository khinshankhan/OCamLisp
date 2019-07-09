let () =
  let argv_list = Array.to_list Sys.argv in
  match argv_list with
  | [a] ->
    Reader.interactive []
  | [a; b] ->
    Reader.noninteractive b []
  | _ -> failwith "Invalid number of arguments"
