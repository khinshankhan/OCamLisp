let sym_extract = function
  | `Sym s -> s
  | _ -> failwith "unaccounted for"

let print_t = function
  | `Bool b -> print_endline "bool"
  | `Char c -> print_char c
  | `Float f -> print_float f
  | `Int i -> print_int i
  | `String s -> Printf.printf "\"%s\"" s
  | `Sym s -> print_endline "sym"
  | `Tuple [] -> print_endline "empty"
  | _ -> failwith "unaccounted for"

let print t =
  print_endline ""; print_t t; print_endline ""
