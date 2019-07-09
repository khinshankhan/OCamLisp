open Batteries
let noninteractive filename env =
  filename |> open_in |> Lexing.from_channel |> Parser.prog Lexer.read |> Eval.eval

let rec interactive env =
  IO.stdin |> IO.read_line |> IO.input_string |> Lexing.from_input |> Parser.prog Lexer.read |> Eval.eval
