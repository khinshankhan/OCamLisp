open Parser
open Lexer

let print_token = function
  | Parser.INT i-> print_endline ("NUMBER " ^ string_of_int i)
  | Parser.SYM s-> print_endline ("SYMBOL " ^ s)
  | Parser.STRING s-> print_endline ("STRING " ^ s)
  | Parser.LPAREN -> print_endline ("LPAREN (")
  | Parser.RPAREN -> print_endline ("RPAREN )")
  | Parser.EOF -> failwith "yayeet"
  | _ -> print_endline "something else"

let rec lexer lexbuf =
  let token = Lexer.read lexbuf in
  begin
    print_token token;
    lexer lexbuf;
  end

let () =
  try
    let filename = Sys.argv.(1) in
    let lexbuf = Lexing.from_channel (open_in filename) in
    lexer lexbuf
  with
    _ -> exit 0;
