let matcher = function
  | `Bool b -> print_endline "b"
  | `Char c -> print_endline "c"
  | `Float f -> print_endline "f"
  | `Int i -> print_endline "i"
  | `String s -> print_endline "str"
  | `Sym s -> print_endline "sym"
  | `Tuple [] -> print_endline "empty"
  | `Tuple ls -> print_endline "stuff in"

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
  | Sexp.Atom t ->
    begin
      match t with
      | `Int i -> i
      | _ -> failwith "atom fail"
    end
  | _ -> eval_sexp t

let rec eval = function
  | h::t -> print_int (eval_sexp h); print_endline ""; eval t
  | _ -> ()
