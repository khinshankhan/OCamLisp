open Parser
open Lexer

let matcher = function
  | `Bool b -> print_endline "b"
  | `Char c -> print_endline "c"
  | `Float f -> print_endline "f"
  | `Int i -> print_endline "i"
  | `String s -> print_endline "str"
  | `Sym s -> print_endline "sym"
  | `Tuple [] -> print_endline "empty"
  | `Tuple ls -> print_endline "stuff in"


let rec print_sexp = function
  | Sexp.Atom t -> matcher t
  | Sexp.Cons t -> List.iter print_sexp t

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
  List.iter print_sexp sexp
