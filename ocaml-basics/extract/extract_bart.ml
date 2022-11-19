let rec extract i = function
    [] -> failwith "index out of bounds"
  | x::l -> if i=0 then (x,l)
             else let (y,l') = extract (i-1) l
                  in (y, x::l')
;;

extract 0 [1;2;3];;
extract 1 [1;2;3];;
extract 2 [1;2;3];;
extract 3 [1;2;3];;


(*Estrae alla posizione i della lista, il suddetto elemento*)

let rec extr i = function
[] -> failwith "index out of bound"
| h::t -> if i = 0 
  then (h,t) 
else let (y,t') = extr (i-1) t
in (y, h::t');;

extr 0 [1;2;3];;
extr 1 [1;2;3];;
extr 2 [1;2;3];;
extr 3 [1;2;3];;
