let rec f x y = 
  if x y = 0 then f x (y+1)
  else x y
;;

let g z = if z<3 then 0 else z;;

f g 0;;

(*finche z è minore di 3 -> la funzione f va a incrementare y di 1, fino a che f g 0 = 3*)