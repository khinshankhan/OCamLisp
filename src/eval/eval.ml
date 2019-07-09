open Syntax

let op f1 f2 n1 n2 =
  match n1, n2 with
  | `Int x, `Int y -> `Int (f1 x y)
  | `Int x, `Float y -> `Float (f2 (float x) y)
  | `Float x, `Int y -> `Float (f2 x (float y))
  | `Float x, `Float y -> `Float (f2 x y)
  | _ -> failwith "invalid num"

let ascii_to_string n =
  match n with
  | Sexp.Atom `Int i ->
    `String (Printf.sprintf "%c" (Char.chr i))
  | _ -> failwith "invalid sequence"

let rec concat s1 s2 =
  match s1, s2 with
  | `String x, `String y -> `String (x ^ y)
  | `String x, `Tuple xs ->
    let stringified = List.map ascii_to_string xs in
    concat s1 (List.fold_left concat (`String "") stringified)
  | _ -> failwith "invalid"

let rec lookup s = function
  | (sym, value)::t ->
    if s = sym
    then Some value
    else lookup s t
  | [] -> None

let sym_lookup env = function
  | `Sym s ->
    let looked = lookup s env in
    begin
      match looked with
      | Some v -> v
      | None ->
        begin
          match s with
          | "+" -> op ( + ) ( +. )
          | "-" -> op ( - ) ( -. )
          | "*" -> op ( * ) ( *. )
          | "/" -> op ( / ) ( /. )
          | _ -> failwith "symbol not recognized"
        end
    end
  | _ -> failwith "invalid symbol"

let rec sym_ops env a sym =
  let s = sym_extract sym in
  match s with
  | "setq" -> (env, `Tuple [])
  | "print" -> List.iter print a; (env, sym)
  | ("+" | "-" | "*" | "/") ->
    let ret =
      begin
        match a with
        | h::[] ->
          begin
            match s with
            | ("+" | "*") -> h
            | "/" -> `Int 0
            | "-" ->
              let _, value = sym_ops env [h; `Int (-1)] (`Sym "*") in
              value
            | _ -> failwith "impossible"
          end
        | h::t -> List.fold_left (sym_lookup env sym) h t
        | _ ->
          begin
            match s with
            | ("+" | "-") -> `Int 0
            | "*" -> `Int 1
            | "/" -> failwith "Wrong number of arguments: /, 0"
            | _ -> failwith "impossible"
          end
      end
    in
    (env, ret)
  | "concat" ->
    (env, List.fold_left concat (`String "") a)
  | _ -> failwith "unaccounted for"

let rec eval_sexp env = function
  | Sexp.Cons t ->
    begin
      match t with
      | (Sexp.Atom h)::t ->
        let a = List.map (atomizer env) t in
        let a = List.map (fun (_, y) -> y) a in
        sym_ops env a h
      | _ -> failwith "cons fail"
    end
  | _ -> failwith "sexp fail"
and atomizer env = function
  | Sexp.Atom t -> (env, t)
  | t -> eval_sexp env t

let rec eval env = function
  | h::t ->
    let env,_ = eval_sexp env h in
    eval env t
  | _ -> env
