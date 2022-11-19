let rec mem x = function
    [] -> false
  | y::l -> x=y || mem x l;;

let rec dup = function
    [] -> false
  | x::l -> mem x l || dup l;;



  (*Prendo una lista e, tramite funzion di appoggio scorro i singolielementi per confrontarli, 
  e con la funzione principale scorro la lista stessa*)

  
  let rec verify x = function
  [] -> false
  | h::t -> if h=x then true else verify x t;;

  let rec dup = function
  [] -> false
  | h::t -> verify h t || dup t;;

  assert(not (dup [1;2;3;4;5;6;7;8;9;10]));;
  assert(dup [1;2;3;3;4;5;6;7;8]);;