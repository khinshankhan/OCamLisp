open Batteries

let noninteractive filename env =
  let env = filename |> open_in |> Lexing.from_channel |> Parser.prog Lexer.read |> Eval.eval
  in
  ()

let rec interactive env =
  let rec interactive_wrapper env =
    begin
      try
        Printf.printf "> ";
        flush stdout;
        let env = IO.read_line IO.stdin |> IO.input_string |> Lexing.from_input |> Parser.prog Lexer.read |> Eval.eval
        in
        `Tuple env
      with
      | BatInnerIO.No_more_input -> print_endline ""; `String "logout"
      | Failure msg -> `String msg
    end
  in
  match interactive_wrapper env with
  | `Tuple env -> interactive env
  | `String msg -> print_endline msg
  | _ -> print_endline "Something went horribly wrong"
