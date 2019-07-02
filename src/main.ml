open Parser
open Lexer

let interactive =
  ()

let noninteractive filename =
  let channel_name = open_in filename in
  let lexbuf = Lexing.from_channel channel_name in
  let sexp = Parser.prog Lexer.read lexbuf in
  List.iter Eval.print_sexp sexp

let () =
  let argv_list = Array.to_list Sys.argv in
  match argv_list with
  | [a] -> interactive
  | [a; b] -> noninteractive b
  | _ -> failwith "Invalid number of arguments"
