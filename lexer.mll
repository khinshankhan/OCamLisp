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

rule read =
  parse
  | blank    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }

  | "#t" { BOOL true }
  | "#f" { BOOL false }
  | char { CHAR (String.get (lexeme lexbuf) 1) }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | int { INT (int_of_string (lexeme lexbuf)) }
  | float { FLOAT (float_of_string (lexeme lexbuf)) }
  | identifier { SYM (lexeme lexbuf) }

  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }

  | eof { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
