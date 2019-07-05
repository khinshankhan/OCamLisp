let parse channel_name =
  try
    let lexbuf = Lexing.from_channel channel_name in
    let sexp = Parser.prog Lexer.read lexbuf in
    Eval.eval sexp;
    flush stdout
  with _ ->
    exit 0
