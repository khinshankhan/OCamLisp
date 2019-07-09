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
        Some env
      with
      | BatInnerIO.No_more_input -> Printf.printf "\nlogout\n"; None
      | Failure msg -> print_endline msg; None
    end
  in
  match interactive_wrapper env with
  | Some env -> interactive env
  | None -> ()
