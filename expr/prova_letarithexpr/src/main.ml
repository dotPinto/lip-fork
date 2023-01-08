open Ast

type exprval = Bool of bool | Nat of int

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "Not " ^ (string_of_expr e)
  | And(e0,e1) -> (string_of_expr e0) ^ " And " ^ (string_of_expr e1)
  | Or(e0,e1) -> (string_of_expr e0) ^ " Or " ^ (string_of_expr e1)
  | Zero -> "Zero"
  | Succ(e) -> "Succ(" ^ (string_of_expr e) ^ ")"
  | Pred(e) -> "Pred(" ^ (string_of_expr e) ^ ")"
  | IsZero(e) -> "Pred(" ^ (string_of_expr e) ^ ")"
  | Let(x,e1,e2) -> "Let(" ^ x ^ "," ^ (string_of_expr e1) ^ (string_of_expr e2)
  | Var x -> x
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

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True 
  | Not(e) -> let e' = trace1 e in Not(e')
  | And(True,e1) -> e1
  | And(False,_) -> False
  | And(e0,e1) -> let e0' = trace1 e0 in And(e0',e1)
  | Or(True,_) -> True
  | Or(False,e1) -> e1
  | Or(e0,e1) -> let e0' = trace1 e0 in Or(e0',e1)
  | Succ(e) -> let e' = trace1 e in Succ(e')
  | Pred(Zero) -> raise NoRuleApplies
  | Pred(Succ(e)) when is_nv e -> e
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when is_nv e -> False
  | IsZero(e) -> let e' = trace1 e in IsZero(e')
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

let string_of_val = function
    Bool b -> string_of_bool b 
  | Nat n -> string_of_int n

exception TypeError of string

type id = ID of string

type env = id -> exprval

let rec eval = function
    True -> Bool true
  | False -> Bool false
  | If(e0,e1,e2) -> (match eval e0 with
      Bool b -> if b then eval e1 else eval e2
    | _ -> raise (TypeError "If on nat guard")
    )
  | Not(e) -> 
    (match (eval e) with 
        Bool b -> Bool (not b)
      | _ -> raise (TypeError "Not on nat") )
  | And(e0,e1) -> (match (eval e0, eval e1) with
    (Bool b, Bool b1) -> Bool (b&&b1) 
    | _ -> raise (TypeError "And on nat")) 
  | Or(e0,e1) -> (match (eval e0, eval e1) with
    (Bool b, Bool b1) -> Bool (b||b1) 
    | _ -> raise (TypeError "Or on nat")) 
  | Zero -> Nat 0
  | Succ(e) -> (match eval e with 
    Nat n -> Nat (n+1) 
    | _ -> raise (TypeError "Succ on bool"))
  | Pred(e) -> (match eval e with
    Nat n when n>0 -> Nat (n-1)
    | _ -> raise (TypeError "pred on 0"))
  | IsZero(e) -> (match eval e with
    | Nat n -> Bool (n=0)
    | _ -> raise (TypeError "IsZero on bool"))
  | _ -> raise (TypeError "Mancano Var _ e Let(_,_,_)")
;;

