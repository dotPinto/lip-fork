%{
open Ast
%}

%token LET
%token EQ
%token IN
%token <string> ID
%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token IF
%token THEN
%token ELSE
%token ZERO
%token SUCC
%token PRED
%token ISZERO
%token LPAREN
%token RPAREN
%token EOF

%nonassoc ELSE
%left OR
%left AND
%nonassoc NOT

%nonassoc SUCC, PRED, ISZERO

%right IN

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | x = ID { Var x }
  | TRUE { True }
  | FALSE { False }
  | NOT; e=expr { Not(e) }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | ZERO { Zero }
  | SUCC; e = expr { Succ(e) }
  | PRED; e = expr { Pred(e) }
  | ISZERO; e = expr { IsZero(e) }
  | LET; x = ID; EQ; e1=expr; IN; e2=expr; { Let(x,e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
