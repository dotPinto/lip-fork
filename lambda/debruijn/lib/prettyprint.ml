open Ast

let rec string_of_namedterm = function
    NamedVar x -> x
  | NamedAbs(x,t) -> "λ" ^ x ^ ". " ^ string_of_namedterm t
  | NamedApp(NamedVar x,NamedVar y) -> x ^ " " ^ y
  | NamedApp(NamedVar x,t2) -> x ^ " (" ^ string_of_namedterm t2 ^ ")"
  | NamedApp(t1,NamedVar x) -> "(" ^ string_of_namedterm t1 ^ ") " ^ x
  | NamedApp(t1,t2) -> "(" ^ string_of_namedterm t1 ^ ") (" ^ string_of_namedterm t2 ^ ")"

let rec string_of_dbterm = function
    DBVar n -> string_of_int n
  | DBAbs t -> "λ. " ^ string_of_dbterm t
  | DBApp(t1,DBApp(t21,t22)) -> string_of_dbterm t1 ^ " (" ^ string_of_dbterm (DBApp(t21,t22)) ^ ")"
  | DBApp(DBAbs t1,DBVar n) -> "(" ^ string_of_dbterm (DBAbs t1) ^ ") " ^  string_of_int n 
  | DBApp(DBAbs t1,t2) -> "(" ^ string_of_dbterm (DBAbs t1) ^ ") (" ^ string_of_dbterm t2 ^ ")" 
  | DBApp(t1,DBAbs t21) -> string_of_dbterm t1 ^ " (" ^ string_of_dbterm (DBAbs t21) ^ ")"
  | DBApp(t1,t2) -> string_of_dbterm t1 ^ " " ^ string_of_dbterm t2

let string_of_trace = function
  | [] -> ""
  | t0::ts ->  string_of_dbterm t0 ^ "\n" ^
  List.fold_right (fun t acc -> "-> " ^ string_of_dbterm t ^ "\n" ^ acc) ts ""
