{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white { read lexbuf }
  | "let" { LET }
  | "in" { IN }
  | "=" { EQ }
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "0" { ZERO }
  | "succ" { SUCC }  
  | "pred" { PRED }
  | "iszero" { ISZERO }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
