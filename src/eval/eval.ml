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
  | _ -> Error._failwith "sym fail"

let rec sym_ops a sym =
  let s = sym_extract sym in
  match s with
  | "print" -> List.iter print a;  sym
  | ("+" | "-" | "*" | "/") ->
    begin
      match a with
      | h::[] ->
        begin
          match s with
          | ("+" | "*") -> h
          | "/" -> `Int 0
          | "-" -> sym_ops [h; `Int (-1)] (`Sym "*")
          | _ -> Error._failwith "impossible"
        end
      | h::t -> List.fold_left (sym_lookup sym) h t
      | _ ->
        begin
          match s with
          | ("+" | "-") -> `Int 0
          | "*" -> `Int 1
          | "/" -> Error._failwith "Wrong number of arguments: /, 0"
          | _ -> Error._failwith "impossible"
        end
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
and atomizer = function
  | Sexp.Atom t -> t
  | t -> eval_sexp t

let rec eval = function
  | h::t -> eval_sexp h; eval t
  | _ -> ()
