open Parser
open Lexer

let () =
  let argv_list = Array.to_list Sys.argv in
  let channel_name =
    begin
      match argv_list with
      | [a] -> stdin
      | [a; b] -> (open_in b)
      | _ -> failwith "Invalid number of arguments"
    end
  in
  let lexbuf = Lexing.from_channel channel_name in
  let sexp = Parser.prog Lexer.read lexbuf in
  List.iter Eval.print_sexp sexp
