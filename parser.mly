%token <bool> BOOL
%token <char> CHAR
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <string> SYM

%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK

%token EOF

%start <Sexp.t list> prog
%%

prog:
  | EOF       { [] }
  | sexp prog { $1 :: $2 }

sexp:
  | atom { Atom $1 }
  | cons { Cons $1 }

atom:
  | BOOL { `Bool $1 }
  | CHAR { `Char $1 }
  | STRING { `String $1 }
  | INT { `Int $1 }
  | FLOAT { `Float $1 }
  | SYM { `Sym $1 }
  | LBRACK elems RBRACK { `Tuple $2 }

cons:
  | LPAREN elems RPAREN { $2 }
