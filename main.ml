let filename = Sys.argv.(1)

let () =
  let inBuffer = open_in filename in
  let lineBuffer = Lexing.from_channel inBuffer in
  try
    let acceptance = Parser.main Lexer.main lineBuffer in
    match acceptance with
    | true -> print_string "Accepted!\n"
    | false -> print_string "Not accepted!\n"
  with
  | Lexer.Error msg -> Printf.fprintf stderr "%s%!\n" msg
  | Parser.Error -> Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lineBuffer)
