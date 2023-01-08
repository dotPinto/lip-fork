open Ast

type exprval = Bool of bool | Nat of int

let string_of_val = function
    Bool b -> if b then "true" else "false"
  | Nat n -> string_of_int n


let rec string_of_expr = function
    True -> "true"
  | False -> "false"
  | Not(e) -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2                    
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ(e) -> "succ(" ^ string_of_expr e ^ ")"
  | Pred(e) -> "pred(" ^ string_of_expr e ^ ")"
  | IsZero(e) -> "iszero(" ^ string_of_expr e ^ ")"
  | Var x -> x
  | Let(x,e1,e2) -> "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2


let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)

exception TypeError of string
exception UnboundVar of string
exception PredOfZero
exception NoRuleApplies

let rec is_nv = function
    Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false


let is_val = function
    True -> true
  | False -> true
  | v when is_nv v -> true
  | _ -> false


let rec subst x e e' = match e' with
    If(e0,e1,e2) -> If(subst x e e0,subst x e e1,subst x e e2)
  | Not(e0) -> Not(subst x e e0)
  | And(e1,e2) -> And(subst x e e1,subst x e e2)
  | Or(e1,e2) -> Or(subst x e e1,subst x e e2)
  | Succ(e1) -> Succ(subst x e e1)
  | Pred(e1) -> Pred(subst x e e1)
  | IsZero(e1) -> IsZero(subst x e e1)
  | Var(y) when y=x -> e
  | Let(y,e1,e2) when y<>x -> Let(y,subst x e e1,subst x e e2)
  | _ -> e'


let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 e in Not(e')
  | And(True,e) -> e
  | And(False,_) -> False
  | And(e1,e2) -> let e1' = trace1 e1 in And(e1',e2)
  | Or(True,_) -> True
  | Or(False,e) -> e
  | Or(e1,e2) -> let e1' = trace1 e1 in Or(e1',e2)
  | Succ(e) -> let e' = trace1 e in Succ(e')
  | Pred(Zero) -> raise NoRuleApplies
  | Pred(Succ(e)) when is_nv e -> e
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when is_nv e -> False    
  | IsZero(e) -> let e' = trace1 e in IsZero(e')
  | Var(_) -> raise NoRuleApplies
  | Let(x,e1,e2) when is_val e1 -> subst x e1 e2
  | Let(x,e1,e2) -> let e1' = trace1 e1 in Let(x,e1',e2)
  | _ -> raise NoRuleApplies


let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec last = function
    [] -> failwith "last on empty list"
  | [x] -> x
  | _::l -> last l

let rec val_of_expr = function
    True -> Some (Bool true)
  | False -> Some (Bool false)
  | Zero -> Some (Nat 0)
  | Succ(e) -> (match val_of_expr e with
      | Some (Nat n) -> Some (Nat (n+1))
      | _ -> None)
  | _ -> None

let eval_smallstep e = val_of_expr (last (trace e))
    

(******************************************************************************)
(*                            Big-step semantics (eager)                      *)
(******************************************************************************)

let bot = fun x -> raise (UnboundVar x)

let bind f x v = fun y -> if y=x then v else f y

let rec eval_rec e rho = match e with
    True -> Bool true
  | False -> Bool false
  | Not(e) -> (match eval_rec e rho with
        Bool b -> Bool(not b)
      | _ -> raise (TypeError "Not on nat")
    )
  | And(e1,e2) -> (match (eval_rec e1 rho,eval_rec e2 rho) with
        (Bool b1,Bool b2) -> Bool (b1 && b2)
      | _ -> raise (TypeError "Or on nat")
    )
  | Or(e1,e2) -> (match (eval_rec e1 rho,eval_rec e2 rho) with
        (Bool b1,Bool b2) -> Bool (b1 || b2)
      | _ -> raise (TypeError "Or on nat")
    ) 
  | If(e0,e1,e2) -> (match eval_rec e0 rho with
        Bool b -> if b then eval_rec e1 rho else eval_rec e2 rho
      | _ -> raise (TypeError "If on nat guard")
    )
  | Zero -> Nat 0
  | Succ(e) -> (match eval_rec e rho with
        Nat n -> Nat (n+1)
      | _ -> raise (TypeError "Succ on bool")
    )
  | Pred(e) -> (match eval_rec e rho with
      | Nat n when n>0 -> Nat (n-1)
      | _ -> raise (TypeError "pred on 0")
    )
  | IsZero(e) -> (match eval_rec e rho with
      | Nat n -> Bool (n=0)
      | _ -> raise (TypeError "IsZero on bool")
    )
  | Var(x) -> rho x
  | Let(x,e1,e2) -> let v1 = eval_rec e1 rho in eval_rec e2 (bind rho x v1)


let eval (e:expr) =  eval_rec e bot

