let rec rev = function
    [] -> []
  | x::l -> (rev l)@[x]
;;


let rec rev_list l = 
  match l with
  [] -> []
  | h::t -> (rev_list t)@[h];;