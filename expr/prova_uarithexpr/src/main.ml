open Ast

type exprval = Bool of bool | Nat of int

let rec string_of_expr = function
    True -> "true"
  | False -> "false"
  | If(e0,e1,e2) -> "if(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "not " ^ (string_of_expr e)
  | And(e0,e1) -> (string_of_expr e0) ^ " and " ^ (string_of_expr e1)
  | Or(e0,e1) -> (string_of_expr e0) ^ " or " ^ (string_of_expr e1)
  | Zero -> "zero"
  | Succ(e) -> "succ(" ^ (string_of_expr e) ^ ")"
  | Pred(e) -> "pred(" ^ (string_of_expr e) ^ ")"
  | IsZero(e) -> "pred(" ^ (string_of_expr e) ^ ")"
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let rec is_nv = function
    Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false

  let oneOrZero = function
  Zero -> false
 |Pred(Zero) -> false
 | _ -> true

let rec trace1 = function
 True -> Succ(Zero)
| False -> Zero
| If(Succ(Zero),e1,_) -> e1
| If(Zero,_,e2) -> e2 
| If(True,e1,_) -> e1
| If(False,_,e2) -> e2
| If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
| Or(Succ(Zero),_) -> Succ(Zero)
| Or(Zero,e1) when oneOrZero e1 -> Succ(Zero)
| Or(True,_) -> True
| Or(False,e1) when oneOrZero e1 -> Succ(Zero) 
| Or(e0,e1) -> let e0' = trace1 e0 in Or(e0',e1)
| And(True,False) -> Zero
| And(Succ(Zero),e1) when oneOrZero e1-> Succ(Zero)
| And(Zero,_) -> Zero
| And(True,e1) when oneOrZero e1 -> Succ(Zero) 
| And(False,_) -> False
| And(e0,e1) -> let e0' = trace1 e0 in And(e0',e1)
| Not(Zero) -> Succ(Zero)
| Not(Succ(Zero)) -> Zero
| Not(True) -> False
| Not(False) -> True
| Not(e0) -> let e0' = trace1 e0 in Not(e0')
| Succ(e) -> let e' = trace1 e in Succ(e')
| Pred(Zero) -> Zero
| Pred(Succ(e)) when is_nv e -> e
| Pred(e) -> let e' = trace1 e in Pred(e')
| IsZero(Zero) -> Succ(Zero)
| IsZero(Succ(e)) when is_nv e -> False  
| IsZero(e) -> let e' = trace1 e in IsZero(e')
| _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

let string_of_val n = 
  string_of_int n
;;

let rec expand = function
True -> Bool true
| False -> Bool false
| If(e0,e1,e2) -> (match expand e0 with
    Bool b -> if b then expand e1 else expand e2
  | Nat n -> if n = 1 then expand e1 else expand e2
  )
| Not(e) -> 
  (match (expand e) with 
      Bool b -> Bool (not b)
    | Nat n -> if n=0 then Bool true else Bool false )
| And(e0,e1) -> (match (expand e0, expand e1) with
    (Bool b, Bool b1) -> Bool (b&&b1) 
   | (Nat n, Nat n1) -> if n>0&&n1>0 then Bool true else Bool false
   | (Bool b, Nat n) -> if n>0 then Bool b else Bool false
   | (Nat n, Bool b) -> if n>0 then Bool b else Bool false
   ) 
| Or(e0,e1) -> (match (expand e0, expand e1) with
      (Bool b, Bool b1) -> Bool (b||b1) 
    | (Nat n, Nat n1) when n>0||n1>0 -> Bool true
    | (Bool b, Nat n)
    | (Nat n, Bool b) -> Bool (b || (n>0))
    | _ -> Bool false
  )
| Zero -> Nat 0
| Succ(e) -> (match expand e with 
    Nat n -> Nat (n+1) 
    | Bool b -> if b then Bool true else Bool false
    )
| Pred(e) -> (match expand e with
    Nat n -> Nat (if n>0 then n-1 else 0)
  | Bool b -> if b then Nat 0 else Nat 1)
| IsZero(e) -> (match expand e with
  | Nat n -> if n=0 then Nat 1 else Nat 0
  | Bool b -> if b then Nat 1 else Nat 0
  )
;;

let rec eval = function
    True -> 1

  | False -> 0

  | If(e0,e1,e2) -> (match expand e0 with
      Bool b -> if b then eval e1 else eval e2
    | Nat n -> if n=0 then eval e2 else eval e1
    )

  | Not(e) -> (match (expand e) with 
        Bool b -> if b then 1 else 0
      | Nat b -> if b=0 then 1 else 0
      )

  | And(e0,e1) -> (match (expand e0, expand e1) with
      (Bool b, Bool b1) -> if b&&b1 then 1 else 0
    | (Nat n, Bool b) -> if b&&(n>0) then 1 else 0
    | (Bool b, Nat n) -> if b&&(n>0) then 1 else 0
    | (Nat n, Nat n1) -> if (n>0)&&(n1>0) then 1 else 0 ) 
  
  | Or(e0,e1) -> (match (expand e0, expand e1) with
      (Bool b, Bool b1) -> if b||b1 then 1 else 0 
    | (Nat n, Bool b)
    | (Bool b, Nat n) -> if b||(n=1) then 1 else 0
    | (Nat n, Nat n1) -> if n>0||n1>0 then 1 else 0
    ) 

  | Zero -> 0

  | Succ(e) -> (match expand e with 
      Nat n -> n+1
    | Bool b -> if b then 1 else 0)

  | Pred(e) -> (match expand e with
      Nat n -> if n>0 then n-1 else 0
    | Bool b -> if b then 0 else 1
    )

  | IsZero(e) -> (match expand e with
    | Nat n -> if n=0 then 1 else 0
    | Bool b -> if b then 0 else 1)
;;

