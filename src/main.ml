let () =
  let rec main env = function
    | [] ->
      Reader.interactive env; ()
    | a::[] ->
      Reader.noninteractive a env; ()
    | h::t ->
      let env = Reader.noninteractive h env in
      main env t
  in
  main [] (Sys.argv |> Array.to_list |> List.tl)
