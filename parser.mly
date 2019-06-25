%token <bool> BOOL
%token <char> CHAR
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <string> SYM

%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET

%token EOF

%start <Sexp.t list> prog
%%

prog:
  | EOF       { [] }
  | sexp prog { $1 :: $2 }

sexp:
  | atom { Atom $1 }
  | cons { Cons $1 }
