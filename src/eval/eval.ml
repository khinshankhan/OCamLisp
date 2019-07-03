type wrapper =
    Bool of bool
  | Char of char
  | Float of float
  | Int of int
  | String of string
  | Sym of string
  | Tuple of wrapper list
  | Nil

let rec convert = function
  | `Bool b -> Bool b
  | `Char c -> Char c
  | `Float f -> Float f
  | `Int i -> Int i
  | `String s -> String s
  | `Sym s -> Sym s
  | `Tuple [] -> Nil
  | `Tuple ls -> Tuple (List.map convert ls)

let matcher = function
  | Bool b -> print_endline "b"
  | Char c -> print_endline "c"
  | Float f -> print_endline "f"
  | Int i -> print_endline "i"
  | String s -> print_endline "str"
  | Sym s -> print_endline "sym"
  | Nil -> print_endline "empty"
  | Tuple ls -> print_endline "stuff in"

let sym_lookup = function
  | `Sym s ->
    (match s with
     | "+" -> (+)
     | "-" -> (-)
     | _ -> failwith "sym fail 2")
  | _ -> failwith "sym fail "

let rec eval_sexp = function
  | Sexp.Cons t ->
    begin
      match t with
      | (Sexp.Atom h)::t ->
        let a = List.map atomizer t in
        List.fold_right (sym_lookup h) a 0
      | _ -> failwith "cons fail"
    end
  | _ -> failwith "sexp fail"
and atomizer t =
  match t with
  | Sexp.Atom t -> convert t
  | _ -> eval_sexp t

let rec eval = function
  | h::t -> matcher (eval_sexp h); print_endline ""; eval t
  | _ -> ()
