let rec lookup s = function
  | (sym, value)::t ->
    if s = sym
    then Some value
    else lookup s t
  | _ -> None

let sym_extract = function
  | `Sym s -> s
  | _ -> failwith "unaccounted for"

let print_t = function
  | `Bool b ->
    begin
      match b with
      | true -> print_endline "#t"
      | false -> print_endline "#f"
    end
  | `Char c -> print_char c
  | `Float f -> print_float f
  | `Int i -> print_int i
  | `String s -> Printf.printf "\"%s\"" s
  | `Sym s -> print_endline s
  | `Tuple [] -> print_endline "Nil"
  | `Tuple xs -> print_endline "listo"
  | _ -> failwith "unaccounted for"

let print t =
  print_endline ""; print_t t; print_endline ""
