open Batteries

let read channel_name =
  channel_name |> Lexing.from_input |> Parser.prog Lexer.read
