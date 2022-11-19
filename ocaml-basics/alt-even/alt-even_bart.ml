let alt_even x =
  let rec alt_even_rec x b = 
    if x=0 then b
    else if (x mod 2 = 0) != b then false else alt_even_rec (x/10) (not b)
  in alt_even_rec x true;;

assert(alt_even 1234);;
assert(not (alt_even 1234567));;


(*La versione del prof utilizza un booleano per definire 
in che tipo di posizione si trova della cifra,
se si trova in posizione pari b = true; false altrimenti

La mia versione utilizza un contatore*)

let rec alt_even x = 
  let rec alt_even_count x c =
    if x = 0 then true else
    if x mod 2 = 0 && c mod 2 = 0 then alt_even_count (x/10) (c+1)
    else if x mod 2 = 1 && c mod 2 = 1 then alt_even_count (x/10) (c+1)
    else false 
  in alt_even_count x 0;;

assert (alt_even 0);;
assert(alt_even 1234);;
assert(not (alt_even 1234567));;
assert(alt_even 12345678);;
assert(not (alt_even 123364567));;