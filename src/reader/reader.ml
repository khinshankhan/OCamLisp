open Batteries

let noninteractive filename env =
  let env = filename |> open_in |> Lexing.from_channel |> Parser.prog Lexer.read |> Eval.eval
  in
  ()

let rec interactive_wrapper env =
  begin
    try
      Printf.printf "> ";
      flush stdout;
      IO.read_line IO.stdin |> IO.input_string |> Lexing.from_input |> Parser.prog Lexer.read |> Eval.eval
      |> interactive
    with
    | BatInnerIO.No_more_input -> print_endline "logout"
    | Failure msg -> print_endline msg
  end
and interactive env =
  interactive_wrapper env
