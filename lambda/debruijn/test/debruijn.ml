open DebruijnLib.Ast
open DebruijnLib.Main
open DebruijnLib.Prettyprint

let c0 = "fun s. fun z. z"
let c1 = "fun s. fun z. s z"
let c2 = "fun s. fun z. s (s z)"
let plus = "fun m. fun n. fun s. fun z. m s (n s z)"

let test_parse = [
  ("fun x. x", DBAbs (DBVar 0));
  ("fun z. z", DBAbs (DBVar 0));
  (c0, DBAbs (DBAbs (DBVar 0)));
  (c1, DBAbs (DBAbs (DBApp (DBVar 1, DBVar 0))));
  (c2, DBAbs (DBAbs (DBApp (DBVar 1, DBApp (DBVar 1, DBVar 0)))));
  (plus, DBAbs
    (DBAbs
      (DBAbs
        (DBAbs
          (DBApp (DBApp (DBVar 3, DBVar 1),
            DBApp (DBApp (DBVar 2, DBVar 1), DBVar 0)))))));
  ("(fun x. fun x. x) (fun y. y)", DBApp (DBAbs (DBAbs (DBVar 0)), DBAbs (DBVar 0)));
  ("x", DBVar 0);
  ("a b", DBApp (DBVar 0, DBVar 1));
  ("b a", DBApp (DBVar 1, DBVar 0));
  ("fun z. x y z", DBAbs (DBApp (DBApp (DBVar 1, DBVar 2), DBVar 0)));
  ("fun y . y x (fun z . y z x w) w z y", DBAbs
    (DBApp
      (DBApp
        (DBApp
          (DBApp (DBApp (DBVar 0, DBVar 2),
            DBAbs (DBApp (DBApp (DBApp (DBVar 1, DBVar 0), DBVar 3), DBVar 2))),
          DBVar 1),
        DBVar 3),
      DBVar 0)))
]

let%test _ =
  print_newline ();
  print_endline ("*** Testing parse...");
  List.fold_left
    (fun b (ts,t) ->
      let ts' = parse ts in
      print_string (ts ^ " => " ^ string_of_dbterm ts');
      let b' = ts' = t in
      print_string (" " ^ (if b' then "[OK]" else "[NO : expected " ^ string_of_dbterm t ^ "]"));
      print_newline();
      b && b')
    true
    test_parse

let plusone = "(" ^ plus ^ ") (" ^ c1 ^ ")"
let c3 = "(" ^ plusone ^ ") (" ^ c2 ^ ")"
let c4 = "(" ^ plus ^ ") (" ^ c2 ^ ") (" ^ c2 ^ ")"

let omega = "(fun x. x x) (fun x. x x)"

let test_trace = [
  ("(fun x. y x z) (fun x. x)",1,"y (fun c. c) z");
  ("(fun u. fun v. u x) y",10,"fun v. y x");
  (plusone,1,"fun n. fun s. fun z. (fun s. fun z. s z) s (n s z)");
  (c3,10,"fun s. fun z. s (s (s z))");
  (c4,10,"fun s. fun z. s (s (s (s z)))");
  (omega,5,omega)
]

let rec last = function
    [] -> failwith "last on empty list"
  | [x] -> x
  | _::l -> last l
              
let%test _ =
  print_newline();
  print_endline ("*** Testing trace...");  
  List.fold_left
    (fun b (ts,n,ts') ->
       let t = parse ts and t' = parse ts' in
       let ar = last (trace n t) in (* actual result *)
       print_string (ts ^ " --" ^ string_of_int n ^ "-> " ^ string_of_dbterm ar);
       let b' = ar = t' in
       print_string (" " ^ (if b' then "[OK]" else "[NO : expected " ^ string_of_dbterm t' ^ "]"));
       print_newline();
       b && b')
    true
    test_trace
