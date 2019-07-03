type wrapper =
    Bool of bool
  | Char of char
  | Float of float
  | Int of int
  | String of string
  | Sym of string
  | Nil

let rec sym_to_wrapper = function
  | `Bool b -> Bool b
  | `Char c -> Char c
  | `Float f -> Float f
  | `Int i -> Int i
  | `String s -> String s
  | `Sym s -> Sym s
  | `Tuple [] -> Nil
  | _ -> failwith "unaccounted for"

let print_wrapper = function
  | Bool b -> print_endline "b"
  | Char c -> print_char c
  | Float f -> print_float f
  | Int i -> print_int i
  | String s -> print_endline "str"
  | Sym s -> print_endline "sym"
  | Nil -> print_endline "empty"
