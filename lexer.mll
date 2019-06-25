{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let blank = [' ' '\t' ',']
let newline = '\n' | '\r' | "\n\r"
let char = '\\'

let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let int = '-'? ['0'-'9'] ['0'-'9']*
let float = '-'?['0'-'9']*'.'?['0'-'9']+
let symbols = ['*' '+' '!' '-' '_' '?' '<' '>' '=' '/' '%' '^' '~']
let alnum = alpha | int | float

let identifier = (alpha | symbols) (alnum | symbols)*
let char = '\\' _
