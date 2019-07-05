open Syntax

let op f1 f2 n1 n2 =
  match n1, n2 with
  | `Int x, `Int y -> `Int (f1 x y)
  | `Int x, `Float y -> `Float (f2 (float x) y)
  | `Float x, `Int y -> `Float (f2 x (float y))
  | `Float x, `Float y -> `Float (f2 x y)
  | _ -> failwith "invalid num"

let sym_lookup = function
  | `Sym s ->
    (match s with
     | "+" -> op ( + ) ( +. )
     | "-" -> op ( - ) ( -. )
     | _ -> failwith "sym fail 2")
  | _ -> failwith "sym fail "

let sym_ops a sym =
  match (sym_extract sym) with
  | ("+" | "-") -> List.fold_right (sym_lookup sym) a (`Int 0)
  | _ -> failwith "unaccounted for"

let rec eval_sexp = function
  | Sexp.Cons t ->
    begin
      match t with
      | (Sexp.Atom h)::t ->
        let a = List.map atomizer t in
        sym_ops a h
      | _ -> failwith "cons fail"
    end
  | _ -> failwith "sexp fail"
and atomizer t =
  match t with
  | Sexp.Atom t -> t
  | _ -> eval_sexp t

let rec eval = function
  | h::t -> print_t (eval_sexp h); print_endline ""; eval t
  | _ -> ()
