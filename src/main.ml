open Batteries

let () =
  let argv_list = Array.to_list Sys.argv in
  match argv_list with
  | [a] ->
    IO.stdin |> IO.read_line |> IO.input_string |> Lexing.from_input |> Parser.prog Lexer.read |> Eval.eval
  | [a; b] ->
    b |> open_in |> Lexing.from_channel |> Parser.prog Lexer.read |> Eval.eval
  | _ -> Error._failwith "Invalid number of arguments"
