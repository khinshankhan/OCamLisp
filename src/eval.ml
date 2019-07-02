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
