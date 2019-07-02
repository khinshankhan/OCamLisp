type t =
  | Atom of [ `Int of int | `Float of float | `Bool of bool | `String of string | `Char of char | `Sym of string | `Tuple of t list]
| Cons of t list
