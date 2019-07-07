open Syntax

let op f1 f2 n1 n2 =
  match n1, n2 with
  | `Int x, `Int y -> `Int (f1 x y)
  | `Int x, `Float y -> `Float (f2 (float x) y)
  | `Float x, `Int y -> `Float (f2 x (float y))
  | `Float x, `Float y -> `Float (f2 x y)
  | _ -> Error._failwith "invalid num"

let ascii_to_string n =
  match n with
  | Sexp.Atom `Int i ->
    `String (Printf.sprintf "%c" (Char.chr i))
  | _ -> Error._failwith "invalid sequence"
let rec concat s1 s2 =
  match s1, s2 with
  | `String x, `String y -> `String (x ^ y)
  | `String x, `Tuple xs ->
    let stringified = List.map ascii_to_string xs in
    concat s1 (List.fold_left concat (`String "") stringified)
  | _ -> Error._failwith "invalid"

let sym_lookup = function
  | `Sym s ->
    (match s with
     | "+" -> op ( + ) ( +. )
     | "-" -> op ( - ) ( -. )
     | "*" -> op ( * ) ( *. )
     | "/" -> op ( / ) ( /. )
     | _ -> Error._failwith "sym fail 2")
  | _ -> Error._failwith "sym fail "

let sym_ops a sym =
  match (sym_extract sym) with
  | "print" -> List.iter print a;  sym
  | ("+" | "-" | "*" | "/") ->
    begin
      match a with
      | h::t -> List.fold_left (sym_lookup sym) h t
      | _ -> Error._failwith "invalid number of arguments for this operation"
    end
  | "concat" ->
     List.fold_left concat (`String "") a
  | _ -> Error._failwith "unaccounted for"

let rec eval_sexp = function
  | Sexp.Cons t ->
    begin
      match t with
      | (Sexp.Atom h)::t ->
        let a = List.map atomizer t in
        sym_ops a h
      | _ -> Error._failwith "cons fail"
    end
  | _ -> Error._failwith "sexp fail"
and atomizer t =
  match t with
  | Sexp.Atom t -> t
  | _ -> eval_sexp t

let rec eval = function
  | h::t -> eval_sexp h; eval t
  | _ -> ()
