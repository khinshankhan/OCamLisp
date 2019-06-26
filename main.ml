let filename = Sys.argv.(1)

let () =
  let lexbuf = Lexing.from_string "(+ 5 7)" in
  let result = Parser.prog Lexer.read lexbuf in
  print_endline result
