module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

(* mzip dmn cdm : string list -> b' list -> a' StringMap.t is the StringMap that
   maps the strings of dmn to the elements of cdm in the order they appear *)
let rec mzip xs ys = match xs,ys with
  | [],[] -> StringMap.empty
  | [],_::_ | _::_,[] -> failwith "Cannot zip: lists do no match in size!"
  | x::xs',y::ys' -> StringMap.add x y (mzip xs' ys')

(* range a b is the integer interval [a;b) encoded as a list *)
let rec range a b = if a < b then a::(range (a+1) b) else []
