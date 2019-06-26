let filename = Sys.argv.(1)

let () =
  let in_channel = In_channel.create filename in
  let lexbuf = Lexing.from_channel in_channel in
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close in_channel;
