%{
open Ast
%}

%token <string> VAR
%token ABS
%token DOT
%token LPAREN
%token RPAREN
%token EOF

%start <namedterm> prog

%nonassoc DOT ABS
%nonassoc LPAREN VAR 
%left APP

%%

prog:
  | t = term; EOF { t }
;

term:
  | x = VAR { NamedVar x }
  | ABS; x = VAR; DOT; t = term { NamedAbs(x,t) }
  | LPAREN; t=term; RPAREN { t }
  | t1=term; t2=term { NamedApp(t1,t2) } %prec APP
;
